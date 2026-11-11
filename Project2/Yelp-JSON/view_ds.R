library(dplyr)
library(tidyr)
library(stringr)

# Run from MATH167R, Project2, or Yelp-JSON.
project_dirs <- c("Project2/Yelp-JSON", "Yelp-JSON", ".")
data_files <- file.path(
  project_dirs, "data", "new_orleans", "business_new_orleans.rds"
)
matching_file <- which(file.exists(data_files))[1]

if (is.na(matching_file)) {
  stop("Run this script from MATH167R, Project2, or Project2/Yelp-JSON.")
}

businesses <- readRDS(data_files[matching_file])

# Each business can have several comma-separated category labels.
# Split them, standardize capitalization/spacing, and count each business once
# per category. Missing or empty labels are excluded from category counts.
business_categories <- businesses |>
  select(business_id, categories) |>
  filter(!is.na(categories)) |>
  separate_longer_delim(categories, delim = ",") |>
  mutate(category = str_to_lower(str_squish(categories))) |>
  filter(category != "") |>
  distinct(business_id, category)

category_counts <- business_categories |>
  count(category, name = "number_of_businesses", sort = TRUE)

# Also count labels among restaurants for the restaurant-focused assignment.
# These include cuisine labels and other labels, such as bars and shopping.
restaurant_ids <- business_categories |>
  filter(category == "restaurants") |>
  distinct(business_id)

restaurants <- businesses |>
  semi_join(restaurant_ids, by = "business_id")

restaurant_category_counts <- business_categories |>
  semi_join(restaurant_ids, by = "business_id") |>
  count(category, name = "number_of_restaurants", sort = TRUE)

dataset_summary <- tibble(
  number_of_businesses = nrow(businesses),
  distinct_categories = nrow(category_counts),
  businesses_without_categories = sum(
    is.na(businesses$categories) | str_squish(businesses$categories) == ""
  ),
  number_of_restaurants = nrow(restaurants),
  distinct_labels_among_restaurants = nrow(restaurant_category_counts)
)

print(dataset_summary, width = Inf)
cat("\nTop 20 categories across all businesses:\n")
print(head(category_counts, 20))
cat("\nTop 20 category labels among restaurants:\n")
print(head(restaurant_category_counts, 20))

# Open searchable tables when sourced in an interactive R session.
if (interactive()) {
  View(businesses)
  View(category_counts)
  View(restaurant_category_counts)
}
