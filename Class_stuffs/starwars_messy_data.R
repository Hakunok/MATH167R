library(tidyverse)

# ── First look ────────────────────────────────────────────────────────────────

starwars

glimpse(starwars)

# How many columns? What types?
ncol(starwars)
names(starwars)


# ── Finding missing data ──────────────────────────────────────────────────────

# Count NAs in every column

starwars |>
  summarise( #collapses a dataframe into 1 row
    name1       = sum(is.na(name)),
    height1     = sum(is.na(height)),
    mass1       = sum(is.na(mass)),
    hair_color1 = sum(is.na(hair_color)),
    skin_color1 = sum(is.na(skin_color)),
    eye_color1  = sum(is.na(eye_color)),
    birth_year1 = sum(is.na(birth_year)),
    sex1        = sum(is.na(sex)),
    gender1     = sum(is.na(gender)),
    homeworld1  = sum(is.na(homeworld)),
    species1    = sum(is.na(species)),
    films1      = sum(is.na(films)),
    vehicles1   = sum(is.na(vehicles)),
    starships1  = sum(is.na(starships))
  )

# Look at rows where hair_color is missing
starwars |>
  filter(is.na(hair_color)) |>
  select(name, species, hair_color)

# Look at rows where birth_year is missing
starwars |>
  filter(is.na(birth_year)) |>
  select(name, birth_year)


# ── Filtering out NAs ─────────────────────────────────────────────────────────

# Only characters with known height and mass
starwars |>
  filter(!is.na(height), !is.na(mass)) |>
  select(name, height, mass)

# How many are dropped?
nrow(starwars)
starwars |> filter(!is.na(height), !is.na(mass)) |> nrow()

starwars |>
  summarise(
    avg_height1 = mean(height),
    avg_mass1   = mean(mass),
    avg_height2 = mean(height, na.rm = TRUE),
    avg_mass2   = mean(mass, na.rm = TRUE)
  )

# ── Replacing NAs ─────────────────────────────────────────────────────────────

# Replace missing hair_color with "none/unknown"
starwars |>
  mutate(hair_color1 = replace_na(hair_color, "unknown")) |>
  mutate(hair_color2 = ifelse(is.na(hair_color), "unknown", hair_color)) |>
  mutate(hair_color3 = case_when(  is.na(hair_color) ~ "unknown",  .default = hair_color)) |>
  mutate(hair_color4 = coalesce(hair_color, "unknown")) |>

  filter(is.na(hair_color)) |>
  select(hair_color, hair_color1, hair_color2, hair_color3, hair_color4)

# Replace missing birth_year with the median birth year
median_birth <- median(starwars$birth_year, na.rm = TRUE)

# You better have a really good reason for doing this... I would prefer to remove NA
starwars |>
  mutate(birth_year = replace_na(birth_year, median_birth)) |>
  summarise(mean_birth = mean(birth_year))


# ── Grouping with missing data ────────────────────────────────────────────────

# Average height by species (na.rm drops NAs within the mean calculation)
starwars |>
  group_by(species) |>
  summarise(
    n = n(),
    avg_height = mean(height, na.rm = TRUE)
  ) |>
  arrange(desc(n))

# Which species has the highest mass on average?
starwars |>
  filter(!is.na(mass)) |>
  group_by(species) |>
  summarise(avg_mass = mean(mass)) |>
  arrange(desc(avg_mass))


# ── String cleaning ───────────────────────────────────────────────────────────

# Eye colors are inconsistent — what values exist?
starwars |> count(eye_color, sort = TRUE)

# Group unusual eye colors together
starwars |>
  mutate(eye_color_grouped = case_when(
    eye_color %in% c("blue", "blue-gray") ~ "blue",
    eye_color %in% c("brown", "dark")     ~ "brown/dark",
    eye_color == "yellow"                 ~ "yellow",
    .default                              ~ "other"
  )) |>
  count(eye_color_grouped)

# Find characters whose name contains a space (multi-word names)
starwars |>
  filter(str_detect(name, " ")) |>
  select(name)

# Extract first name only
starwars |>
  mutate(first_name = str_extract(name, "^\\S+")) |>
  select(name, first_name)


# ── Dealing with list columns ─────────────────────────────────────────────────

# films, vehicles, starships are list columns — each cell holds a vector
starwars |> select(name, films) |> head(3)

# unnest to get one row per character-film combination
starwars |>
  select(name, films) |>
  unnest(films) |>
  count(films, sort = TRUE)

# Which characters appear in the most films?
starwars |>
  select(name, films) |>
  unnest(films) |>
  count(name, sort = TRUE)


# ── BMI: combining columns + handling NAs ─────────────────────────────────────

starwars |>
  mutate(
    height_m = height / 100,
    bmi      = mass / (height_m ^ 2)
  ) |>
  filter(!is.na(bmi)) |>
  select(name, height, mass, bmi) |>
  arrange(desc(bmi))
