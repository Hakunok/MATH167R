# Packages used in this lesson
# install.packages("rnaturalearth")
# install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "tigris"))

library(ggplot2)

data(cars)


# Basic scatterplot
ggplot(data=cars, mapping = aes(x = speed, y=dist)) +
  geom_point()


# Mapping variables to visual properties
ggplot(cars, aes(
  x = speed,
  y = dist,
  color = speed,
  size = dist
)) +
  geom_point()

# Mapping versus setting a fixed color
ggplot(cars ) +
  geom_point(aes(speed, dist, color = speed))

ggplot(cars, aes(speed, dist)) +
  geom_point(color = "navy", size = 3)

# Line graph
cars_sorted <- cars |> arrange(speed)

ggplot(cars_sorted, aes(x = speed, y = dist)) +
  geom_line()

# Trend line
ggplot(cars, aes(x = speed, y = dist)) +
  geom_smooth(method = "lm", se = FALSE)

# Bar chart: counts observations
ggplot(cars, aes(x = speed)) +
  geom_bar(fill = "steelblue")

# Bar chart using calculated values
average_distance <- cars |>
  group_by(speed) |>
  summarise(dist = mean(dist))

ggplot(average_distance, aes(x = speed, y = dist)) +
  geom_col(fill = "darkorange")

# Histogram
ggplot(cars, aes(x = dist)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 5,
    fill = "skyblue",
    color = "black"
)

# Density plot
ggplot(cars, aes(x = dist)) +
  geom_density(fill = "purple", alpha = 0.4)

# Create speed groups
cars <- cars |>
  mutate(speed_group = case_when(
    speed <= 10 ~ "Slow",
    speed <= 20 ~ "Medium",
    speed <= 30 ~ "Fast"
  ))

# Boxplot
ggplot(cars, aes(y = dist)) +
  geom_boxplot(fill = "lightgreen")

# Boxplot by speed group
ggplot(cars, aes(x = speed_group, y = dist)) +
  geom_boxplot(fill = "lightblue")

# Violin plot
ggplot(cars, aes(x = speed_group, y = dist)) +
  geom_violin(fill = "orange")

# Jitter plot
ggplot(cars, aes(x = speed_group, y = dist)) +
  geom_jitter(width = 0.15, alpha = 0.6, color = "darkblue")

# Text labels
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_text(aes(label = speed), vjust = -0.7, size = 3)

# Labels inside boxes
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_label(aes(label = speed), size = 3)

# Horizontal reference line
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_hline(
    yintercept = cars |> pull(dist) |> mean(),
    color = "red",
    linetype = "dashed"
  )


# Rug marks
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_rug()

# Combination: points and trend line
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "red"
  )

# Combination: observations and group means
group_means <- cars |>
  group_by(speed_group) |>
  summarise(dist = mean(dist))

ggplot(cars, aes(x = speed_group, y = dist)) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  geom_point(
    data = group_means,
    aes(x = speed_group, y = dist),
    color = "red",
    size = 4
  )

# Three geometries on one graph
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "red",
    linewidth = 1
  ) +
  geom_rug(color = "darkgreen", alpha = 0.6) +
  labs(
    title = "Three Geometries in One Graph",
    subtitle = "Points, trend line, and rug marks",
    x = "Car Speed",
    y = "Stopping Distance"
  ) +
  theme_minimal()

# Labels
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  labs(
    title = "Speed and Stopping Distance",
    subtitle = "Data from the built-in cars dataset",
    x = "Speed of the Car",
    y = "Stopping Distance",
    caption = "Source: R built-in cars dataset"
  )

# Continuous axis scale
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point()  +
  scale_x_continuous(
    breaks = seq(0, 30, by = 5),
    limits = c(0, 35)
  )

# Manual colors
ggplot(cars, aes(x = speed_group, y = dist, fill = speed_group)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "Slow" = "skyblue",
      "Medium" = "gold",
      "Fast" = "tomato"
    )
  )

# Color gradient
ggplot(cars, aes(x = speed, y = dist, color = dist)) +
  geom_point(size = 3) +
  scale_color_gradient(
    low = "navy",
    high = "orange"
  )


# Built-in themes
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  theme_minimal()

ggplot(cars, aes(speed, dist)) +
  geom_point() +
  theme_classic()

ggplot(cars, aes(speed, dist)) +
  geom_point() +
  theme_bw()

ggplot(cars, aes(speed, dist)) +
  geom_point() +
  theme_void()

# Customized theme
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Spatial packages
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tigris)

options(tigris_use_cache = TRUE)

# World map
world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

ggplot(world) +
  geom_sf(fill = "lightgreen", color = "white", linewidth = 0.2) +
  labs(title = "World Map") +
  theme_void()

# United States map
united_states <- world[world$name == "United States of America", ]

ggplot(united_states) +
  geom_sf(fill = "lightblue", color = "navy") +
  labs(title = "United States") +
  theme_void()

# California counties
california_counties <- counties(
  state = "CA",
  cb = TRUE,
  class = "sf"
)

ggplot(california_counties) +
  geom_sf(fill = "gray90", color = "white") +
  labs(title = "California Counties") +
  theme_void()

# Saratoga, California
california_places <- places(
  state = "CA",
  cb = FALSE,
  class = "sf"
)

saratoga <- california_places[
  grepl("^Saratoga", california_places$NAME, ignore.case = TRUE),
]

ggplot(saratoga) +
  geom_sf(fill = "khaki", color = "black") +
  labs(title = "Saratoga, California") +
  theme_void()

# Roads and streams near Saratoga
ca_roads <- roads(
  state = "CA",
  county = "Santa Clara",
  year = 2022,
  class = "sf"
)

ca_streams <- linear_water(
  state = "CA",
  county = "Santa Clara",
  year = 2022,
  class = "sf"
)

saratoga_geometry <- st_union(saratoga)

roads_near_saratoga <- st_intersection(ca_roads, saratoga_geometry)
streams_near_saratoga <- st_intersection(ca_streams, saratoga_geometry)

# Detailed Saratoga map
ggplot() +
  geom_sf(
    data = saratoga,
    fill = "gray95",
    color = "black"
  ) +
  geom_sf(
    data = streams_near_saratoga,
    color = "steelblue",
    linewidth = 0.5
  ) +
  geom_sf(
    data = roads_near_saratoga,
    color = "gray35",
    linewidth = 0.25
  ) +
  labs(
    title = "Saratoga, California: Roads and Water Features",
    subtitle = "Spatial layers overlaid with geom_sf()"
  ) +
  theme_void()

# Check coordinate reference systems
st_crs(saratoga)
st_crs(ca_roads)

# Transform a layer when coordinate systems do not match
ca_roads <- st_transform(
  ca_roads,
  st_crs(saratoga)
)
