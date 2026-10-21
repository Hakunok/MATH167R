library(sf)	
library(ggplot2)	
library(rnaturalearth)	
library(rnaturalearthdata)	
world <- ne_countries(	
    scale = "medium",	
    returnclass = "sf"
)	
names(world)

# Estimated GDP is measured in millions of US dollars.
# Treat nonpositive values (including missing-data codes) as unavailable.
world$gdp_md[world$gdp_md <= 0] <- NA_real_

gdp_map <- ggplot(world) +
    geom_sf(aes(fill = gdp_md), color = "white", linewidth = 0.15) +
    scale_fill_viridis_c(
        option = "C",
        trans = "log10",
        breaks = c(100, 1000, 10000, 100000, 1000000, 10000000),
        labels = scales::label_comma(),
        name = "GDP (million US$)",
        na.value = "grey85"
    ) +
    labs(
        title = "Estimated GDP per country (in millions)",
        subtitle = "Total GDP in million US dollars; logarithmic color scale",
        caption = paste(
            "Source: Natural Earth. GDP estimate years vary by country.",
            "Gray indicates unavailable data."
        )
    ) +
    theme_void() +
    theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5),
        plot.caption = element_text(size = 9, hjust = 0.5),
        plot.margin = margin(15, 15, 15, 15)
    )

print(gdp_map)

ggsave(
    filename = "hw3_gdp_map.png",
    plot = gdp_map,
    width = 12,
    height = 7,
    dpi = 300,
    bg = "white"
)

# I used three places I want to visit for future vacations as the subjects for this
# Approximate coordinates; Rovaniemi represents the Lapland region.
# Uyuni refers to the city, and Seljalandsfoss is the waterfall.
important_places <- data.frame(	
place = c("Finland", "Bolivia", "Iceland"),	
city = c("Lapland", "Uyuni", "Seljalandsfoss"),	
longitude = c(25.7333, -66.8239, -19.9886),	
latitude = c(66.5000, -20.4628, 63.6156)	
)	
important_places_sf <- st_as_sf(	
important_places,	
coords = c("longitude", "latitude"),	
crs = 4326	
)

# Connect the locations through Iceland to keep the route easy to follow.
# This line is a visual connection, rather than an actual travel route.
connection_order <- c(1, 3, 2)
connecting_line <- st_sfc(
    st_linestring(
        as.matrix(important_places[connection_order, c("longitude", "latitude")])
    ),
    crs = 4326
)

city_colors <- c(
    "Lapland" = "#0066CC",
    "Uyuni" = "#006633",
    "Seljalandsfoss" = "#990066"
)
city_shapes <- c("Lapland" = 16, "Uyuni" = 17, "Seljalandsfoss" = 15)

# Keep the original GDP map and create a second version with the places.
gdp_places_map <- gdp_map +
    geom_sf(data = connecting_line, color = "white", linewidth = 1.2) +
    geom_sf(
        data = connecting_line,
        color = "grey25",
        linewidth = 0.6,
        linetype = "dashed"
    ) +
    geom_sf(
        data = important_places_sf,
        aes(shape = city),
        color = "white",
        size = 5,
        show.legend = FALSE
    ) +
    geom_sf(
        data = important_places_sf,
        aes(color = city, shape = city),
        size = 3.5
    ) +
    geom_label(
        data = important_places,
        aes(x = longitude, y = latitude, label = city, color = city),
        nudge_x = c(8, 7, -8),
        nudge_y = c(9, -7, -13),
        hjust = c(0, 0, 1),
        fill = "white",
        size = 3.5,
        linewidth = 0.2,
        show.legend = FALSE
    ) +
    scale_color_manual(name = "Selected locations", values = city_colors) +
    scale_shape_manual(name = "Selected locations", values = city_shapes) +
    guides(
        fill = guide_colorbar(order = 1),
        color = guide_legend(order = 2),
        shape = guide_legend(order = 2)
    ) +
    labs(
        caption = paste(
            "Source: Natural Earth. GDP estimate years vary; gray indicates unavailable data.",
            "\nLapland is represented by Rovaniemi. Dashed line connects the selected locations."
        )
    )

print(gdp_places_map)

ggsave(
    filename = "hw3_places_map.png",
    plot = gdp_places_map,
    width = 14,
    height = 8,
    dpi = 300,
    bg = "white"
)
