# R Midterm Review
# Run after placing the CSV files in the working directory.


# Run once if necessary:
# install.packages("tidyverse")

library(tidyverse)
set.seed(167)
reviews <- read_csv("restaurant_reviews.csv")
licenses <- read_csv("restaurant_licenses.csv")
glimpse(reviews)
head(reviews)
names(reviews)
dim(reviews)
summary(reviews)
colSums(is.na(reviews))
reviews |>
  count(review_id) |>
  filter(n > 1)

reviews |>
  count(state, sort = TRUE)

write_csv(reviews, "reviews_clean.csv")
reviews |>
  filter(rating >= 4) |>
  select(restaurant_name, rating) |>
  arrange(desc(rating))
arrange(
  select(filter(reviews, rating >= 4), restaurant_name, rating),
  desc(rating)
)
reviews |> select(restaurant_name, city, rating)
reviews |> select(restaurant_name, starts_with("review"))
reviews |> select(-review_text)
reviews |> filter(rating >= 4)
reviews |> filter(rating >= 4, city == "San Luis Obispo")
reviews |> filter(city == "Morro Bay" | rating == 5)
reviews |> filter(city %in% c("Morro Bay", "San Luis Obispo"))
reviews_small <- reviews |>
  slice(1:7) |>
  select(review_id, restaurant_name, rating)

reviews_small$rating[c(1, 3)] <- NA

reviews_small
reviews_small |> filter(is.na(rating))
reviews_small |> filter(!is.na(rating))
# NA rows are NOT included in this result
reviews_small |> filter(rating < 4)
reviews_small |> filter(rating >= 4)
# NA poisons the whole result
sum(reviews_small$rating)

# na.rm = TRUE ignores missing values
sum(reviews_small$rating, na.rm = TRUE)
reviews |> arrange(desc(rating), restaurant_name)

reviews |>
  mutate(
    rating_percent = rating / 5 * 100,
    excellent = rating == 5
  )

reviews |>
  summarize(
    number_of_reviews = n(),
    average_rating = mean(rating, na.rm = TRUE),
    highest_rating = max(rating, na.rm = TRUE)
  )
reviews |>
  group_by(restaurant_name) |>
  summarize(
    number_of_reviews = n(),
    average_rating = mean(rating, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(average_rating))
reviews_clean <- reviews |>
  mutate(
    name_clean = restaurant_name |>
      str_to_upper() |>
      str_squish(),
    city_clean = city |>
      str_to_upper() |>
      str_squish(),
    address_clean = address_line |>
      str_to_upper() |>
      str_replace_all("\\.", "") |>
      str_replace_all("\\bSTREET\\b", "ST") |>
      str_replace_all("\\bAVENUE\\b", "AVE") |>
      str_replace_all("\\bBOULEVARD\\b", "BLVD") |>
      str_replace_all("\\bROAD\\b", "RD") |>
      str_squish()
  )
str_to_upper("hello")
str_to_lower("HELLO")
str_trim("  text  ")
str_squish("  too    many   spaces  ")
str_detect("Morro Bay", "Bay")
str_extract("Rating: 5 stars", "[0-9]+")
str_replace("old name", "old", "new")
str_replace_all("STREET AVENUE", "STREET", "ST")
licenses_clean <- licenses |>
  mutate(
    name_clean = legal_business_name |>
      str_to_upper() |>
      str_squish() |>
      str_replace_all("\\bCO\\.?\\b", "COMPANY") |>
      str_replace_all("\\bBBQ\\b", "BARBECUE"),
    address_clean = str_c(
      street_number, street_name, street_type, sep = " "
    ) |>
      str_to_upper() |>
      str_squish(),
    city_clean = city_name |>
      str_to_upper() |>
      str_squish(),
    join_key = str_c(
      name_clean, address_clean, city_clean, zip_code, sep = "|"
    )
  )

reviews_clean <- reviews_clean |>
  mutate(
    name_clean = name_clean |>
      str_replace_all("\\bCO\\.?\\b", "COMPANY") |>
      str_replace_all("\\bBBQ\\b", "BARBECUE"),
    join_key = str_c(
      name_clean, address_clean, city_clean, postal_code, sep = "|"
    )
  )
# Keep every review, adding license information when available
review_license <- reviews_clean |>
  left_join(licenses_clean, by = "join_key")

# Keep only records that matched in both tables
matched_records <- reviews_clean |>
  inner_join(licenses_clean, by = "join_key")

# Keep every record from both tables
all_records <- reviews_clean |>
  full_join(licenses_clean, by = "join_key")

# Reviews with no matching license
reviews_clean |> anti_join(licenses_clean, by = "join_key")

# Licenses with no matching review
licenses_clean |> anti_join(reviews_clean, by = "join_key")
licenses_clean |>
  count(join_key) |>
  filter(n > 1)
ggplot(reviews, aes(x = rating)) +
  geom_histogram(binwidth = 1, boundary = 0) +
  labs(
    title = "Distribution of Restaurant Ratings",
    x = "Rating",
    y = "Number of reviews"
  )
reviews |>
  group_by(review_date) |>
  summarize(avg_rating = mean(rating, na.rm = TRUE)) |>
  ggplot(aes(x = review_date, y = avg_rating)) +
  geom_line() +
  labs(
    title = "Average Rating Over Time",
    x = "Date",
    y = "Average rating"
  )

cars <- tibble(
  speed = c(4, 4, 7, 7, 8),
  dist = c(2, 10, 4, 22, 16)
)

ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  labs(
    title = "Car Speed vs. Stopping Distance",
    x = "Speed (mph)",
    y = "Stopping distance (ft)"
  )
square <- function(x) {
  x^2
}

square(5)

percent_change <- function(old_value, new_value) {
  (new_value - old_value) / old_value * 100
}

percent_change(80, 92)
sample_mean <- function(x) {
  mean(x, na.rm = TRUE)
}
for (i in 1:5) {
  print(i)
}

for (restaurant in c("A", "B", "C")) {
  print(paste("Reviewing", restaurant))
}
results <- numeric(5)

for (i in 1:5) {
  results[i] <- i^2
}

results
simulate_mean <- function(n, B = 1000) {
  results <- numeric(B)

  for (b in 1:B) {
    results[b] <- mean(rexp(n, rate = 1))
  }

  results
}

sample_means <- simulate_mean(n = 10, B = 1000)
dnorm(0, mean = 0, sd = 1)
pnorm(1.96, mean = 0, sd = 1)
qnorm(0.975, mean = 0, sd = 1)
rnorm(10, mean = 0, sd = 1)
# Exactly 3 successes in 10 trials
dbinom(3, size = 10, prob = 0.4)

# Probability of at least 3 successes
1 - pbinom(2, size = 10, prob = 0.4)

# Probability of waiting more than 5 time units
pexp(5, rate = 0.5, lower.tail = FALSE)

# Generate 1000 exponential observations
x <- rexp(1000, rate = 1)
c(mean = mean(x), median = median(x), sd = sd(x), iqr = IQR(x))
set.seed(167)

B <- 10000
results <- numeric(B)

for (b in 1:B) {
  sample_values <- rexp(10, rate = 1)
  results[b] <- mean(sample_values)
}


simulated_mean <- mean(results)
simulated_sd <- sd(results)

# Dates and date-times
today <- Sys.Date()
today
as.integer(today)

now <- Sys.time()
now
as.integer(now)

library(lubridate)

ymd("2026-01-15")
mdy("January 15, 2026")
dmy("15-01-2026")
ymd_hms("2026-01-15 08:30:00")

d <- ymd("2026-10-07")
year(d)
month(d)
day(d)
week(d)
wday(d, label = TRUE)
yday(d)
quarter(d)

d <- ymd("2026-10-07")
d + days(10)
d - weeks(2)
d + months(3)
d + years(1)

start <- ymd("2026-01-01")
end <- ymd("2026-10-07")
end - start
as.numeric(end - start)
as.numeric(end - start) / 7

reviews |>
  mutate(
    year = year(review_date),
    month = month(review_date, label = TRUE),
    day_of_week = wday(review_date, label = TRUE)
  ) |>
  select(restaurant_name, review_date, year, month, day_of_week)

