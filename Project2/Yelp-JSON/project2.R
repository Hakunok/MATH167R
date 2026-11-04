# Install once if needed:
# install.packages(c("tidyverse", "jsonlite"))

library(tidyverse)
library(jsonlite)

# Locate the data when running from MATH167R, Project2, or Yelp-JSON.
project_dirs <- c("Project2/Yelp-JSON", "Yelp-JSON", ".")
business_files <- file.path(
  project_dirs, "Yelp JSON", "yelp_dataset",
  "yelp_academic_dataset_business.json"
)
matching_dir <- which(file.exists(business_files))[1]

if (is.na(matching_dir)) {
  stop("Run this script from MATH167R, Project2, or Project2/Yelp-JSON.")
}

business_file <- business_files[matching_dir]
output_dir <- file.path(project_dirs[matching_dir], "data", "new_orleans")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Import businesses and keep all New Orleans businesses, with all columns.
businesses <- stream_in(
  file(business_file),
  verbose = FALSE
)

new_orleans_businesses <- businesses |>
  filter(
    state == "LA",
    str_to_lower(str_squish(city)) == "new orleans"
  ) |>
  distinct(business_id, .keep_all = TRUE)

output_file <- file.path(output_dir, "business_new_orleans.rds")
saveRDS(
  new_orleans_businesses,
  output_file,
  compress = "xz"
)

message("Saved ", nrow(new_orleans_businesses), " New Orleans businesses to: ",
        normalizePath(output_file, winslash = "/"))

# Free the memory used by the full business table
rm(businesses)
gc()
