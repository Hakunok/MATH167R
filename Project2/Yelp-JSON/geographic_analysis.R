# New Orleans restaurants: categories, elevation >= 10th percentile, stars >= 4.
# Run from MATH167R, Project2, or Yelp-JSON. First run needs internet access.
# Public source responses are cached locally; subsequent runs reuse the cache.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(sf)
  library(ggplot2)
})
options(timeout = 120, tigris_use_cache = FALSE)

project_dirs <- c("Project2/Yelp-JSON", "Yelp-JSON", ".")
inputs <- file.path(project_dirs, "data/new_orleans/business_new_orleans.rds")
match_index <- which(file.exists(inputs))[1]
if (is.na(match_index)) stop("Run from MATH167R, Project2, or Yelp-JSON.")
project_dir <- project_dirs[match_index]
cache_dir <- file.path(project_dir, "data/geography")
output_dir <- file.path(project_dir, "results/geography")
figure_dir <- file.path(output_dir, "maps")
for (directory in c(cache_dir, output_dir, figure_dir)) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
}

selected_categories <- c("cajun/creole", "cafes", "arts & entertainment")
elevation_percentile <- 0.10
minimum_stars <- 4
nola_service <- paste0(
  "https://maps.nola.gov/server/rest/services/",
  "Reference/Reference_Layers/MapServer/0"
)
usgs_service <- paste0(
  "https://elevation.nationalmap.gov/arcgis/rest/services/",
  "3DEPElevation/ImageServer"
)

get_json <- function(url) {
  for (attempt in 1:3) {
    result <- tryCatch({
      request_url <- paste0(url, "&attemptKey=", attempt, "-",
                             format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC"))
      response <- curl::curl_fetch_memory(
        request_url, curl::new_handle(connecttimeout = 20, timeout = 90)
      )
      if (response$status_code != 200) stop("HTTP ", response$status_code)
      parsed <- jsonlite::fromJSON(rawToChar(response$content))
      if (!is.null(parsed$error)) stop(jsonlite::toJSON(parsed$error))
      parsed
    }, error = function(e) e)
    if (!inherits(result, "error")) return(result)
    if (attempt < 3) Sys.sleep(attempt)
  }
  stop("Public data request failed: ", conditionMessage(result))
}

businesses <- readRDS(inputs[match_index])
category_rows <- businesses |>
  select(business_id, categories) |>
  filter(!is.na(categories)) |>
  separate_longer_delim(categories, delim = ",") |>
  mutate(category = str_to_lower(str_squish(categories))) |>
  distinct(business_id, category)
restaurant_ids <- category_rows |>
  filter(category == "restaurants") |>
  distinct(business_id)
selected_rows <- category_rows |>
  semi_join(restaurant_ids, by = "business_id") |>
  filter(category %in% selected_categories)
selected_labels <- selected_rows |>
  group_by(business_id) |>
  summarise(matched_categories = paste(sort(category), collapse = "; "),
            .groups = "drop")
selected <- businesses |>
  select(business_id, name, address, city, state, postal_code,
         latitude, longitude, stars, review_count, is_open) |>
  inner_join(selected_labels, by = "business_id")
message("Category group: ", nrow(selected), " restaurants.")
if (anyDuplicated(selected$business_id)) stop("Duplicate business IDs.")

# Official City of New Orleans Neighborhood Statistical Areas.
neighborhood_file <- file.path(cache_dir, "nola_neighborhoods.geojson")
if (!file.exists(neighborhood_file)) {
  message("Downloading official New Orleans neighborhood boundaries.")
  url <- paste0(nola_service,
                "/query?where=1%3D1&outFields=*&outSR=4326&f=geojson")
  response <- curl::curl_fetch_memory(
    url, curl::new_handle(connecttimeout = 20, timeout = 90)
  )
  if (response$status_code != 200) stop("Neighborhood download failed.")
  parsed <- jsonlite::fromJSON(rawToChar(response$content))
  if (!is.null(parsed$error) || isTRUE(parsed$exceededTransferLimit)) {
    stop("Incomplete neighborhood response.")
  }
  writeBin(response$content, neighborhood_file)
}
neighborhoods <- st_read(neighborhood_file, quiet = TRUE) |>
  transmute(area_id = as.character(NEIGH_ID), area_name = GNOCDC_LAB) |>
  st_make_valid() |>
  st_transform(26915)

# Census geographies are cached as RDS files inside the project.
tract_file <- file.path(cache_dir, "orleans_tracts_2024.rds")
if (!file.exists(tract_file)) {
  message("Downloading 2024 Census tract boundaries.")
  tracts_raw <- tigris::tracts(state = "LA", county = "Orleans",
                             year = 2024, cb = TRUE, class = "sf",
                             progress_bar = FALSE)
  saveRDS(tracts_raw, tract_file, compress = "xz")
}
tracts <- readRDS(tract_file) |>
  transmute(area_id = GEOID, area_name = paste("Tract", NAME)) |>
  st_make_valid() |>
  st_transform(26915)
place_file <- file.path(cache_dir, "new_orleans_place_2024.rds")
if (!file.exists(place_file)) {
  message("Downloading the official 2024 New Orleans place boundary.")
  places <- tigris::places(state = "LA", year = 2024, cb = TRUE,
                           class = "sf", progress_bar = FALSE)
  city_boundary <- places |> filter(NAME == "New Orleans")
  if (nrow(city_boundary) != 1) stop("Could not identify New Orleans place.")
  saveRDS(city_boundary, place_file, compress = "xz")
}
city_boundary <- readRDS(place_file) |> st_make_valid() |> st_transform(26915)

# Query all selected restaurants before applying the rating filter.
# The USGS service returns bare-earth elevation in meters. Request provenance
# fields and save responses in an incremental coordinate-keyed cache.
elevation_file <- file.path(cache_dir, "restaurant_elevations.rds")
elevation_cache <- if (file.exists(elevation_file)) readRDS(elevation_file) else {
  tibble(business_id = character(), longitude = double(), latitude = double(),
         elevation_m = double(), source_resolution = double(),
         raster_id = integer(), vertical_datum = character(),
         source_name = character(), queried_utc = character())
}
valid_coordinates <- selected |>
  filter(is.finite(longitude), is.finite(latitude),
         between(longitude, -180, 180), between(latitude, -90, 90))
pending <- valid_coordinates |>
  anti_join(elevation_cache, by = c("business_id", "longitude", "latitude"))
if (nrow(pending) > 0) {
  batches <- split(seq_len(nrow(pending)),
                   ceiling(seq_len(nrow(pending)) / 1))
  download_state <- new.env()
  download_state$cache <- elevation_cache
  download_state$failures <- list()
  save_elevation_chunk <- function(response, chunk, batch_number) {
    if (!is.null(response$error)) stop(jsonlite::toJSON(response$error))
    samples <- response$samples
    chunk$elevation_m <- NA_real_
    chunk$source_resolution <- NA_real_
    chunk$raster_id <- NA_integer_
    chunk$vertical_datum <- NA_character_
    chunk$source_name <- NA_character_
    if (!is.null(samples) && nrow(samples) > 0) {
      # ArcGIS locationId is zero-based; validate the mapping to input points.
      locations <- as.integer(samples$locationId) + 1L
      if (anyDuplicated(locations) ||
          any(locations < 1L | locations > nrow(chunk))) {
        stop("Unexpected USGS sample indexing.")
      }
      if (any(abs(samples$location$x - chunk$longitude[locations]) > 1e-7) ||
          any(abs(samples$location$y - chunk$latitude[locations]) > 1e-7)) {
        stop("USGS response coordinates do not match the requested points.")
      }
      chunk$elevation_m[locations] <- suppressWarnings(as.numeric(samples$value))
      chunk$source_resolution[locations] <- samples$resolution
      chunk$raster_id[locations] <- samples$rasterId
      if (!is.null(samples$attributes$VerticalDatum)) {
        chunk$vertical_datum[locations] <- samples$attributes$VerticalDatum
      }
      if (!is.null(samples$attributes$Name)) {
        chunk$source_name[locations] <- samples$attributes$Name
      }
    }
    chunk$queried_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    download_state$cache <- bind_rows(
      download_state$cache,
      chunk |> select(all_of(names(download_state$cache)))
    )
    saveRDS(download_state$cache, elevation_file, compress = "xz")
    message("Elevation batch ", batch_number, "/", length(batches), " saved.")
  }
  # Four concurrent connections, with each completed response cached immediately.
  # Small requests avoid the public service's long-URL failures.
  pool <- curl::new_pool(total_con = 4, host_con = 4)
  batch_urls <- character(length(batches))
  for (batch_number in seq_along(batches)) {
    chunk <- pending[batches[[batch_number]], ]
    geometry <- jsonlite::toJSON(
      list(points = as.matrix(chunk[c("longitude", "latitude")]),
           spatialReference = list(wkid = 4326)),
      auto_unbox = TRUE, digits = 12
    )
    batch_urls[batch_number] <- paste0(
      usgs_service, "/getSamples?f=json&geometryType=esriGeometryMultipoint",
      "&returnFirstValueOnly=true&interpolation=RSP_BilinearInterpolation",
      "&outFields=*&geometry=", utils::URLencode(geometry, reserved = TRUE)
    )
    callbacks <- local({
      this_chunk <- chunk
      this_batch <- batch_number
      list(
        done = function(result) {
          tryCatch({
            if (result$status_code != 200) stop("HTTP ", result$status_code)
            save_elevation_chunk(
              jsonlite::fromJSON(rawToChar(result$content)),
              this_chunk, this_batch
            )
          }, error = function(e) {
            download_state$failures[[as.character(this_batch)]] <-
              conditionMessage(e)
          })
        },
        fail = function(message) {
          download_state$failures[[as.character(this_batch)]] <- message
        }
      )
    })
    curl::curl_fetch_multi(
      batch_urls[batch_number], done = callbacks$done, fail = callbacks$fail,
      handle = curl::new_handle(connecttimeout = 20, timeout = 90), pool = pool
    )
  }
  invisible(curl::multi_run(pool = pool))
  if (length(download_state$failures)) {
    for (key in names(download_state$failures)) {
      batch_number <- as.integer(key)
      message("Retrying elevation batch ", batch_number, ".")
      retry_url <- paste0(batch_urls[batch_number], "&cacheKey=",
                          format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC"))
      save_elevation_chunk(get_json(retry_url),
                           pending[batches[[batch_number]], ], batch_number)
    }
  }
  elevation_cache <- download_state$cache
}
selected <- selected |>
  left_join(elevation_cache, by = c("business_id", "longitude", "latitude"))
if (!any(is.finite(selected$elevation_m))) stop("No elevations available.")
elevation_cutoff <- unname(quantile(selected$elevation_m,
                                  probs = elevation_percentile,
                                  na.rm = TRUE, type = 7))
selected <- selected |>
  mutate(
    elevation_ft = elevation_m * 3.280839895,
    qualifies = !is.na(stars) & stars >= minimum_stars &
      is.finite(elevation_m) & elevation_m >= elevation_cutoff
  )

# Use strict containment, without snapping unmatched locations to a boundary.
points <- selected |>
  filter(business_id %in% valid_coordinates$business_id) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  st_transform(26915)
assign_area <- function(points, polygons, prefix) {
  matches <- st_within(points, polygons)
  if (any(lengths(matches) > 1)) stop("Overlapping areas require investigation.")
  index <- vapply(matches, function(m) if (length(m)) m[1] else NA_integer_,
                  integer(1))
  result <- tibble(business_id = points$business_id,
                   id = polygons$area_id[index],
                   name = polygons$area_name[index])
  names(result)[2:3] <- paste0(prefix, c("_id", "_name"))
  result
}
assignments <- assign_area(points, neighborhoods, "neighborhood") |>
  left_join(assign_area(points, tracts, "tract"), by = "business_id") |>
  mutate(within_official_city = lengths(st_within(points, city_boundary)) > 0)
selected <- selected |>
  left_join(assignments, by = "business_id")
qualified <- selected |>
  filter(qualifies) |>
  arrange(desc(stars), desc(review_count), desc(elevation_m), business_id) |>
  mutate(rank = row_number())
top10 <- qualified |> slice_head(n = 10)
qualified_points <- st_as_sf(qualified, coords = c("longitude", "latitude"),
                            crs = 4326, remove = FALSE) |> st_transform(26915)
top10_points <- qualified_points |> filter(rank <= 10)

summarise_areas <- function(polygons, prefix, system_name) {
  id_column <- paste0(prefix, "_id")
  totals <- selected |>
    transmute(area_id = .data[[id_column]], stars, elevation_m,
              review_count, qualifies) |>
    group_by(area_id) |>
    summarise(
      category_restaurants = n(),
      qualifying_restaurants = sum(qualifies),
      average_stars = if (any(qualifies)) mean(stars[qualifies]) else NA_real_,
      median_elevation_m = if (any(qualifies))
        median(elevation_m[qualifies]) else NA_real_,
      total_review_count = sum(review_count[qualifies], na.rm = TRUE),
      .groups = "drop"
    )
  result <- polygons |> st_drop_geometry() |> left_join(totals, by = "area_id")
  if (anyNA(totals$area_id)) {
    result <- bind_rows(result,
                        totals |> filter(is.na(area_id)) |>
                          mutate(area_name = "Unmatched"))
  }
  result |>
    mutate(
      across(c(category_restaurants, qualifying_restaurants, total_review_count),
             ~ replace_na(.x, 0L)),
      percent_qualifying = if_else(category_restaurants > 0,
                                   100 * qualifying_restaurants /
                                     category_restaurants, NA_real_),
      geographic_system = system_name
    ) |>
    arrange(desc(qualifying_restaurants), desc(average_stars), area_name)
}
neighborhood_summary <- summarise_areas(neighborhoods, "neighborhood",
                                        "Neighborhood Statistical Area")
tract_summary <- summarise_areas(tracts, "tract", "Census tract (2024)")
geography_comparison <- bind_rows(
  lapply(list(
    list(polygons = neighborhoods, summary = neighborhood_summary,
         id = "neighborhood_id", label = "Neighborhood Statistical Area"),
    list(polygons = tracts, summary = tract_summary,
         id = "tract_id", label = "Census tract (2024)")
  ), function(spec) {
    counts <- spec$summary |> filter(!is.na(area_id))
    tibble(
      geographic_system = spec$label,
      total_areas = nrow(spec$polygons),
      areas_with_category_restaurants = sum(counts$category_restaurants > 0),
      areas_with_qualifying_restaurants = sum(counts$qualifying_restaurants > 0),
      category_restaurants_assigned = sum(!is.na(selected[[spec$id]])),
      category_restaurants_unmatched = sum(is.na(selected[[spec$id]])),
      qualifying_restaurants_assigned = sum(!is.na(qualified[[spec$id]])),
      qualifying_restaurants_unmatched = sum(is.na(qualified[[spec$id]])),
      smallest_occupied_area_count = min(
        counts$category_restaurants[counts$category_restaurants > 0]),
      largest_area_count = max(counts$category_restaurants),
      qualifying_areas_below_5_restaurants = sum(
        counts$qualifying_restaurants > 0 & counts$qualifying_restaurants < 5)
    )
  })
)
category_summary <- selected_rows |>
  left_join(selected |> select(business_id, qualifies), by = "business_id") |>
  group_by(category) |>
  summarise(category_restaurants = n(),
            qualifying_restaurants = sum(qualifies), .groups = "drop")
category_pairs <- bind_rows(lapply(combn(selected_categories, 2, simplify = FALSE),
  function(pair) {
    ids1 <- selected_rows$business_id[selected_rows$category == pair[1]]
    ids2 <- selected_rows$business_id[selected_rows$category == pair[2]]
    tibble(category_1 = pair[1], category_2 = pair[2],
           restaurants_with_both = length(intersect(ids1, ids2)))
  }))

tables <- list(
  selected_restaurants = selected, qualifying_restaurants = qualified,
  top_10_restaurants = top10, neighborhood_summary = neighborhood_summary,
  census_tract_summary = tract_summary, geography_comparison = geography_comparison,
  category_summary = category_summary, category_cooccurrence = category_pairs
)
for (table_name in names(tables)) {
  write.csv(tables[[table_name]], file.path(output_dir, paste0(table_name, ".csv")),
            row.names = FALSE, na = "", fileEncoding = "UTF-8")
}
saveRDS(list(settings = list(categories = selected_categories,
                            elevation_percentile = elevation_percentile,
                            minimum_stars = minimum_stars,
                            elevation_cutoff_m = elevation_cutoff),
             tables = tables), file.path(output_dir, "analysis_results.rds"),
        compress = "xz")

# Maps share a local metric projection and show the full selected-group extent.
bbox <- st_bbox(points)
padding <- c(1500, 1500)
map_xlim <- c(bbox["xmin"] - padding[1], bbox["xmax"] + padding[1])
map_ylim <- c(bbox["ymin"] - padding[2], bbox["ymax"] + padding[2])
theme_set(theme_minimal(base_size = 12))
map_theme <- theme(
  panel.grid.major = element_line(color = "#dce5e9", linewidth = 0.2),
  plot.title = element_text(face = "bold", color = "#163443", size = 17),
  plot.subtitle = element_text(color = "#425c68", size = 11),
  plot.caption = element_text(color = "#546871", hjust = 0),
  legend.position = "bottom", panel.background = element_rect(
    fill = "#edf4f7", color = NA), axis.title = element_blank()
)
filter_subtitle <- sprintf(
  "Stars >= 4 | Ground elevation >= %.2f m (category-group 10th percentile)",
  elevation_cutoff
)
make_count_map <- function(polygons, area_table, title, filename, source) {
  map_data <- polygons |>
    left_join(area_table |> select(area_id, qualifying_restaurants),
              by = "area_id") |>
    mutate(qualifying_restaurants = replace_na(qualifying_restaurants, 0L),
           map_count = if_else(qualifying_restaurants > 0,
                               qualifying_restaurants, NA_integer_))
  p <- ggplot() +
    geom_sf(data = map_data, aes(fill = map_count),
            color = "#8c9ca4", linewidth = 0.22) +
    geom_sf(data = qualified_points, color = "#172b36", size = 0.65, alpha = 0.55) +
    scale_fill_viridis_c(option = "C", na.value = "#f8fafb",
                         name = "Qualifying restaurants", direction = -1) +
    coord_sf(crs = st_crs(points), xlim = map_xlim, ylim = map_ylim,
             expand = FALSE) +
    labs(title = title, subtitle = filter_subtitle,
         caption = paste0(
           "Dots: qualifying restaurants. White polygons: zero qualifying restaurants.\n",
           source, " | Elevation: USGS 3DEP | Restaurants: local Yelp snapshot.\n",
           "View cropped to category-group locations; full boundary layers are cached."
         )) + map_theme
  ggsave(file.path(figure_dir, filename), p, width = 12, height = 8.8,
         dpi = 180, device = ragg::agg_png, bg = "white")
}
make_count_map(neighborhoods, neighborhood_summary,
               "Where qualifying restaurants are located: neighborhoods",
               "neighborhoods.png", "Boundaries: City of New Orleans NSAs")
make_count_map(tracts, tract_summary,
               "Where qualifying restaurants are located: census tracts",
               "census_tracts.png", "Boundaries: U.S. Census 2024, 1:500,000")

# Numbered top-10 locations, with an explicit downtown-focused extent.
top_bbox <- st_bbox(top10_points)
top_xlim <- c(top_bbox["xmin"] - 1100, top_bbox["xmax"] + 1100)
top_ylim <- c(top_bbox["ymin"] - 1100, top_bbox["ymax"] + 1100)
top_map <- ggplot() +
  geom_sf(data = neighborhoods, fill = "#f4f0e7",
          color = "#a0a7a7", linewidth = 0.35) +
  geom_sf(data = top10_points, aes(fill = stars), shape = 21,
          size = 6.5, color = "#163443", stroke = 0.7) +
  geom_sf_text(data = top10_points, aes(label = rank), size = 3,
               color = "#102a35", fontface = "bold") +
  scale_fill_gradient(low = "#e2b34b", high = "#94d7b7", limits = c(4, 5),
                       name = "Yelp stars") +
  coord_sf(crs = st_crs(points), xlim = top_xlim, ylim = top_ylim, expand = FALSE) +
  labs(title = "Top 10 qualifying restaurants",
       subtitle = "Ranked by stars, then review count, then ground elevation",
       caption = paste0(
         "Numbers match the top-10 table. Boundaries: City of New Orleans NSAs.\n",
         filter_subtitle, " | Elevation is ground height, not dining-floor height."
       )) + map_theme
ggsave(file.path(figure_dir, "top_10_restaurants.png"), top_map,
       width = 12, height = 8.8, dpi = 180, device = ragg::agg_png, bg = "white")

scatter <- ggplot(selected, aes(elevation_m, stars)) +
  geom_vline(xintercept = elevation_cutoff, linetype = "dashed",
              color = "#9d4f23") +
  geom_hline(yintercept = minimum_stars, linetype = "dashed",
              color = "#9d4f23") +
  geom_point(aes(color = qualifies, size = review_count), alpha = 0.6) +
  scale_color_manual(values = c("FALSE" = "#9caab0", "TRUE" = "#14756c"),
                      labels = c("Does not qualify", "Qualifies"), name = NULL) +
  scale_size_continuous(range = c(1, 6), name = "Yelp review count") +
  labs(title = "Ratings and ground elevation",
       subtitle = filter_subtitle, x = "Ground elevation (meters)",
       y = "Yelp business stars",
       caption = "Each point is one restaurant. Dashed lines show the two inclusive cutoffs.") +
  theme(plot.title = element_text(face = "bold", color = "#163443"),
        legend.position = "bottom")
ggsave(file.path(figure_dir, "ratings_and_elevation.png"), scatter,
       width = 12, height = 7, dpi = 180, device = ragg::agg_png, bg = "white")

# Self-contained, offline HTML report with embedded maps and searchable tables.
escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}
html_table <- function(data, id) {
  data <- as.data.frame(data)
  for (column in names(data)) {
    if (is.numeric(data[[column]])) {
      data[[column]] <- ifelse(is.na(data[[column]]), "",
                               format(round(data[[column]], 2), trim = TRUE))
    } else {
      data[[column]] <- ifelse(is.na(data[[column]]), "Unmatched",
                               as.character(data[[column]]))
    }
  }
  header <- paste0("<th>", escape(names(data)), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(data)), function(i) {
    paste0("<tr>", paste0("<td>", escape(unlist(data[i, ], use.names = FALSE)),
                          "</td>", collapse = ""), "</tr>")
  }, character(1))
  paste0('<label class="search">Search this table <input type="search" ',
          'oninput="filterTable(this, &#39;', id,
          '&#39;)" placeholder="Name, neighborhood, category..."></label>',
          '<div class="table-wrap"><table id="', id, '"><thead><tr>',
          header, "</tr></thead><tbody>", paste(rows, collapse = ""),
          "</tbody></table></div>")
}
embed_map <- function(filename, alt) {
  path <- file.path(figure_dir, filename)
  encoded <- jsonlite::base64_enc(readBin(path, "raw", n = file.info(path)$size))
  paste0('<img class="map" alt="', escape(alt),
          '" src="data:image/png;base64,', encoded, '">')
}
area_display <- function(x) {
  x |> transmute(
    Area = area_name, Selected = category_restaurants,
    Qualifying = qualifying_restaurants, Percent = percent_qualifying,
    Average_stars = average_stars, Median_ground_m = median_elevation_m,
    Yelp_review_count = total_review_count
  )
}
restaurant_display <- function(x) {
  x |> transmute(
    Rank = rank, Restaurant = name, Stars = stars, Reviews = review_count,
    Ground_m = elevation_m, Ground_ft = elevation_ft,
    Neighborhood = neighborhood_name, Census_tract = tract_name,
    Categories = matched_categories, Address = address,
    Open_in_Yelp_snapshot = if_else(is_open == 1, "Yes", "No")
  )
}

# Inline SVG keeps each area and restaurant inspectable without web tiles,
# third-party JavaScript, or an internet connection. PNG exports remain available.
write_inspectable_map <- function(polygons, area_table, title, filename) {
  map_data <- polygons |> left_join(area_table |> select(
    area_id, category_restaurants, qualifying_restaurants,
    average_stars, median_elevation_m
  ), by = "area_id")
  map_width <- 1200
  scale <- map_width / diff(map_xlim)
  panel_height <- as.numeric(diff(map_ylim) * scale)
  px <- function(x) as.numeric((x - map_xlim[1]) * scale)
  py <- function(y) as.numeric((map_ylim[2] - y) * scale)
  ring_path <- function(ring) {
    paste0("M ", paste(sprintf("%.2f %.2f", px(ring[, 1]), py(ring[, 2])),
                       collapse = " L "), " Z")
  }
  geometry_path <- function(geometry) {
    parts <- if (inherits(geometry, "POLYGON")) list(geometry) else geometry
    paste(vapply(parts, function(part) {
      paste(vapply(part, ring_path, character(1)), collapse = " ")
    }, character(1)), collapse = " ")
  }
  colors <- grDevices::colorRampPalette(
    c("#f0f921", "#fca636", "#cc4778", "#7e03a8", "#0d0887")
  )(256)
  max_count <- max(map_data$qualifying_restaurants)
  paths <- vapply(seq_len(nrow(map_data)), function(i) {
    row <- st_drop_geometry(map_data[i, ])
    n <- row$qualifying_restaurants
    fill <- if (n == 0) "#f8fafb" else colors[
      1 + round(255 * (n - 1) / max(1, max_count - 1))
    ]
    detail <- paste0(
      row$area_name, " | Selected: ", row$category_restaurants,
      " | Qualifying: ", n,
      if (n > 0) paste0(" | Mean stars: ", sprintf("%.2f", row$average_stars),
                        " | Median ground: ",
                        sprintf("%.2f m", row$median_elevation_m)) else ""
    )
    paste0('<path tabindex="0" aria-label="', escape(detail),
            '" d="', geometry_path(st_geometry(map_data)[[i]]),
            '" fill="', fill, '" fill-rule="evenodd" stroke="#8c9ca4" ',
            'stroke-width="0.8"><title>', escape(detail), '</title></path>')
  }, character(1))
  coords <- st_coordinates(qualified_points)
  dots <- vapply(seq_len(nrow(qualified)), function(i) {
    detail <- paste0(qualified$name[i], " | ", qualified$stars[i],
                      " stars | ", qualified$review_count[i], " reviews | ",
                      sprintf("%.2f m", qualified$elevation_m[i]), " | ",
                      ifelse(is.na(qualified$neighborhood_name[i]), "Unmatched",
                             qualified$neighborhood_name[i]))
    paste0('<circle cx="', px(coords[i, 1]), '" cy="', py(coords[i, 2]),
            '" r="2.5" fill="#193440" opacity=".65"><title>',
            escape(detail), '</title></circle>')
  }, character(1))
  legend_y <- panel_height + 26
  gradient_id <- paste0("gradient-", sub("\\.svg$", "", filename))
  stops <- paste0('<stop offset="', c(0, 25, 50, 75, 100),
                   '%" stop-color="',
                   c("#f0f921", "#fca636", "#cc4778", "#7e03a8", "#0d0887"),
                   '"/>', collapse = "")
  svg <- paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" role="img" ',
    'aria-label="', escape(title), '" viewBox="0 0 ', map_width, " ",
    panel_height + 100, '" style="width:100%;height:auto">',
    '<title>', escape(title), '</title><desc>',
    'Hover a boundary area or a restaurant dot to inspect its data.',
    '</desc><style>path:hover,path:focus{stroke:#105f57;stroke-width:3}',
    'circle:hover{r:5;opacity:1}</style>',
    '<defs><linearGradient id="', gradient_id, '">', stops,
    '</linearGradient><clipPath id="clip-', gradient_id, '"><rect width="',
    map_width, '" height="', panel_height, '"/></clipPath></defs>',
    '<rect width="', map_width, '" height="', panel_height,
    '" fill="#edf4f7"/><g clip-path="url(#clip-', gradient_id, ')">',
    paste(paths, collapse = ""), paste(dots, collapse = ""), '</g>',
    '<rect x="25" y="', legend_y, '" width="260" height="16" fill="url(#',
    gradient_id, ')"/><g fill="#193440" font-family="system-ui,sans-serif" ',
    'font-size="15"><text x="25" y="', legend_y + 38, '">1</text>',
    '<text x="285" y="', legend_y + 38, '" text-anchor="end">', max_count,
    '</text><text x="310" y="', legend_y + 14,
    '">Qualifying restaurants; white areas = zero</text>',
    '<text x="25" y="', legend_y + 63,
    '">Hover an area or dot for details. Ground elevation >= ',
    sprintf("%.3f", elevation_cutoff), ' m; Yelp stars >= 4.</text></g></svg>'
  )
  writeLines(svg, file.path(figure_dir, filename), useBytes = TRUE)
  svg
}
neighborhood_svg <- write_inspectable_map(
  neighborhoods, neighborhood_summary, "Qualifying restaurants by neighborhood",
  "neighborhoods.svg"
)
tract_svg <- write_inspectable_map(
  tracts, tract_summary, "Qualifying restaurants by Census tract",
  "census_tracts.svg"
)

facts <- list(
  selected = nrow(selected), qualified = nrow(qualified),
  missing_elevation = sum(!is.finite(selected$elevation_m)),
  qualified_neighborhoods = n_distinct(na.omit(qualified$neighborhood_id)),
  qualified_tracts = n_distinct(na.omit(qualified$tract_id)),
  closed_qualified = sum(qualified$is_open == 0, na.rm = TRUE),
  outside_city = sum(!selected$within_official_city, na.rm = TRUE)
)
report <- paste0(
'<!doctype html><html lang="en"><head><meta charset="utf-8">',
'<meta name="viewport" content="width=device-width,initial-scale=1">',
'<title>New Orleans restaurants: ratings, elevation, and areas</title>',
'<style>
:root{color-scheme:light}*{box-sizing:border-box}body{margin:0;background:#f3f6f6;color:#193440;font:16px/1.55 system-ui,sans-serif}
main{max-width:1240px;margin:auto;padding:38px 28px 70px}header{border-bottom:3px solid #19786d;padding-bottom:24px;margin-bottom:28px}
.eyebrow{font-size:12px;font-weight:750;letter-spacing:.14em;color:#15766c}h1{font-size:clamp(28px,4vw,44px);line-height:1.16;max-width:950px;margin:12px 0}
h2{font-size:26px;margin:0 0 12px}h3{font-size:20px}p{max-width:1050px}a{color:#126b63}section{background:white;border:1px solid #dce5e5;border-radius:12px;padding:26px;margin:24px 0}
.metrics{display:grid;grid-template-columns:repeat(4,1fr);gap:14px}.metric{background:#eaf3ef;padding:18px;border-radius:9px}.metric strong{display:block;font-size:30px}.metric span{font-size:13px}
.note{border-left:4px solid #c88a39;padding:12px 18px;background:#faf5e9}.map{width:100%;height:auto;border:1px solid #e2e8e8;border-radius:6px}
.table-wrap{overflow:auto;max-height:600px;border:1px solid #dce5e5}table{border-collapse:collapse;width:100%;font-size:13px}th,td{padding:10px 12px;text-align:left;border-bottom:1px solid #e3e9e9;vertical-align:top}
th{background:#eaf1f1;position:sticky;top:0;white-space:nowrap;z-index:1}tr:nth-child(even){background:#f7f9f9}.search{display:block;font-size:13px;margin:12px 0}.search input{padding:9px 12px;border:1px solid #acbfc1;border-radius:5px;margin-left:10px;min-width:260px}
.small,footer{font-size:13px;color:#52676e}nav a{display:inline-block;margin:6px 18px 0 0}code{background:#edf2f1;padding:2px 5px;border-radius:3px}
@media(max-width:700px){main{padding:20px 12px}.metrics{grid-template-columns:repeat(2,1fr)}section{padding:16px}.search input{margin:6px 0;width:100%;min-width:0}}
</style></head><body><main><header><div class="eyebrow">YELP NEW ORLEANS / GEOGRAPHIC ANALYSIS</div>',
'<h1>Restaurants with good ratings and a minimum ground elevation</h1>',
'<p>Cajun/Creole, Cafes, and Arts &amp; Entertainment, connected through Yelp restaurant labels. ',
'Ground elevation at or above the category-group 10th percentile; Yelp business stars at least 4.</p>',
'<nav><a href="#areas">Area tables</a><a href="#maps">Boundary maps</a>',
'<a href="#top10">Top 10</a><a href="#restaurants">All qualifying restaurants</a>',
'<a href="#methods">Methods and sources</a></nav></header>',
'<div class="metrics"><div class="metric"><strong>', facts$selected,
'</strong><span>Restaurants in the category group</span></div><div class="metric"><strong>',
facts$qualified, '</strong><span>Meet both thresholds</span></div><div class="metric"><strong>',
sprintf("%.2f m", elevation_cutoff), '</strong><span>10th-percentile elevation cutoff</span></div>',
'<div class="metric"><strong>', facts$qualified_neighborhoods,
'</strong><span>Neighborhoods with qualifying restaurants</span></div></div>',
'<p class="note"><strong>10th percentile means a lower cutoff.</strong> ',
'It retains roughly 90% of restaurants by elevation before applying the rating filter. ',
'It does not select the highest-elevation 10%. Negative elevations can qualify when the cutoff is negative.</p>',
'<section><h2>Category inclusion and overlap</h2>',
'<p>A business must have the exact <code>Restaurants</code> label and match <strong>at least one</strong> ',
'of the three selected labels. A business matching multiple labels is counted once in the combined group. ',
'Arts &amp; Entertainment describes an experience rather than a cuisine; the overlap counts below make that distinction visible.</p>',
html_table(category_summary, "category-table"),
html_table(category_pairs, "pair-table"), '</section>',
'<section id="areas"><h2>Compare the geographic systems</h2>',
'<p>Counts refer to distinct restaurants. Average stars and median ground elevation describe only restaurants meeting both thresholds. ',
'Average stars gives each restaurant equal weight. Yelp review counts are summed as a volume indicator; they are not reviewer-adjusted ratings.</p>',
html_table(geography_comparison, "comparison-table"),
'<h3>Neighborhood Statistical Areas</h3>',
html_table(area_display(neighborhood_summary), "neighborhood-table"),
'<h3>Census tracts (2024)</h3>',
html_table(area_display(tract_summary), "tract-table"),
'<p class="small">All boundary areas are included, even when Selected and Qualifying are zero. ',
'Percent is the share of selected-category restaurants in that area that qualify. ',
'Any unmatched restaurants are shown separately rather than reassigned to a nearby polygon.</p></section>',
'<section id="maps"><h2>Where the qualifying restaurants are</h2>',
'<p>Hover a polygon to see its area name and statistics, or a dot for restaurant details. ',
'The downloadable PNG versions are saved alongside this report.</p>',
'<h3>Neighborhood Statistical Areas</h3>', neighborhood_svg,
'<h3>Census tracts (2024)</h3>', tract_svg,
'<p>Use neighborhoods to communicate recognizable destinations and census tracts to inspect smaller spatial patterns. ',
'Areas with only a few qualifying restaurants provide limited evidence for an area-wide recommendation. ',
'Maps show restaurant concentrations, not restaurant density per square kilometer.</p>',
'<p><strong>Main-analysis choice: neighborhoods.</strong> They provide recognizable area names ',
'and group restaurants into fewer sparse comparisons. Of the ',
facts$qualified_neighborhoods, ' occupied neighborhoods, ',
geography_comparison$qualifying_areas_below_5_restaurants[1],
' have fewer than five qualifying restaurants; the tract comparison has ',
geography_comparison$qualifying_areas_below_5_restaurants[2], ' such areas among ',
facts$qualified_tracts, ' occupied tracts. Small groups still need careful interpretation.</p></section>',
'<section id="top10"><h2>Top 10 qualifying restaurants</h2>',
'<p>Ranking rule: stars descending, review count descending, then ground elevation descending. ',
'Business ID resolves remaining ties. There is no additional minimum review-count requirement.</p>',
html_table(restaurant_display(top10), "top10-table"),
embed_map("top_10_restaurants.png", "Numbered top ten qualifying restaurant locations"),
'</section><section><h2>How the filters affect the category group</h2>',
embed_map("ratings_and_elevation.png", "Restaurant elevation and star ratings with thresholds"),
'</section><section id="restaurants"><h2>All qualifying restaurants</h2>',
html_table(restaurant_display(qualified), "restaurant-table"), '</section>',
'<section id="methods"><h2>Methods, coverage, and sources</h2><ol>',
'<li><strong>Yelp input:</strong> the complete saved New Orleans business subset (',
nrow(businesses), ' businesses). This subset was filtered using Yelp city = New Orleans and state = LA. ',
'Closed businesses are retained because no current-open restriction was requested; the table shows snapshot status.</li>',
'<li><strong>Categories:</strong> split comma-separated labels, normalize whitespace and capitalization, ',
'and match exact labels. Require Restaurants plus at least one chosen category. ',
'The research theme can be framed as dining with local flavor, cafe culture, or entertainment.</li>',
'<li><strong>Elevation:</strong> query the <a href="', usgs_service,
'">USGS 3DEP bare-earth elevation service</a> at each selected restaurant coordinate using ',
'bilinear interpolation and the first available raster value. Preserve raster ID, source name, vertical datum, ',
'resolution, coordinates, and UTC query time in the cache. Ground elevation is measured in meters; ',
'feet use 3.280839895 feet per meter. Small differences should be interpreted in light of source accuracy.</li>',
'<li><strong>Cutoff population:</strong> all ', facts$selected,
' selected-category restaurants with a finite elevation, before filtering ratings. ',
'Use R quantile type 7 at probability 0.10 (', sprintf("%.6f", elevation_cutoff),
' m), then keep elevation &gt;= cutoff and business stars &gt;= 4. Missing elevations: ',
facts$missing_elevation, '.</li>',
'<li><strong>Neighborhoods:</strong> the City of New Orleans <a href="', nola_service,
'">Neighborhood Statistical Area layer</a>. ',
'<strong>Tracts:</strong> <a href="https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html">',
'U.S. Census 2024 cartographic boundaries</a> for Orleans Parish, downloaded through tigris, at 1:500,000 scale. ',
'These simplified tract/place polygons can leave coastal or boundary-adjacent points unmatched. ',
'Use full TIGER/Line polygons if a boundary-sensitive decision requires greater precision.</li>',
'<li><strong>Spatial assignment:</strong> transform points and polygons to EPSG:26915 and apply strict ',
'<code>sf::st_within()</code>, with no snapping. Compare against the independent 2024 New Orleans place polygon; ',
facts$outside_city, ' selected restaurant coordinates fall outside that simplified city polygon. ',
'Keep these records and report unmatched assignments rather than silently deleting them.</li>',
'<li><strong>Coverage:</strong> qualifying restaurants span ', facts$qualified_neighborhoods,
' neighborhood areas and ', facts$qualified_tracts, ' census tracts. The qualifying set contains ',
facts$closed_qualified, ' businesses marked closed in the Yelp snapshot. ',
'Ratings reflect the downloaded dataset, rather than a live check of current restaurants.</li>',
'<li><strong>Scope:</strong> this report uses Yelp business stars and review counts. ',
'It does not yet implement individual-review or reviewer-normalization analysis required elsewhere in the assignment.</li>',
'</ol><p><strong>Reproduce:</strong> run <code>geographic_analysis.R</code>. ',
'Cached inputs are in <code>data/geography/</code>. CSV tables, analysis RDS, and map PNGs are in ',
'<code>results/geography/</code>. The HTML embeds all figures and works offline.</p></section>',
'<footer>Prepared from the local Yelp snapshot and official public geospatial services. ',
'Query timestamps are preserved in the elevation cache.</footer></main>',
'<script>function filterTable(input,id){const q=input.value.toLowerCase();',
'document.querySelectorAll("#"+id+" tbody tr").forEach(row=>{',
'row.hidden=!row.textContent.toLowerCase().includes(q);});}</script></body></html>'
)
report_file <- file.path(output_dir, "new_orleans_geography.html")
writeLines(enc2utf8(report), report_file, useBytes = TRUE)
writeLines(capture.output(sessionInfo()), file.path(output_dir, "session_info.txt"))
message("DONE: ", facts$qualified, " qualifying restaurants; elevation cutoff ",
        round(elevation_cutoff, 4), " m.")
message("Report: ", normalizePath(report_file, winslash = "/"))
print(geography_comparison, width = Inf)
print(top10 |> select(rank, name, stars, review_count, elevation_m, neighborhood_name))
