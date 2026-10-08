# R DATA WRANGLING CHEAT SHEET
# If the next line gives "there is no package called 'tidyverse'", run this
# once in the Console, then run this script again:
# install.packages("tidyverse")
library(tidyverse)

data <- tibble(
  name   = c("Alice", NA, "John", "Maria", "Sara", "Alice"),
  age    = c(23, NA , 28, 41, 30, 24),
  income = c(48000, 72000, 55000, NA, 61000, 48001),
  state = c("CA", "OR", "TX", "CA", NA, "Ca"),
  score = c(63, 62, 83, 95, NA, 64)
)

data

# -------------------- INSPECTING DATA --------------------

glimpse(data)                         # Structure and column types
summary(data)                         # Summary statistics
names(data)                           # Column names
nrow(data)                            # Number of rows
ncol(data)                            # Number of columns
data |> count(state, sort = TRUE)     # Frequency counts

# -------------------- SELECT AND RENAME --------------------

data |> select(name, age, income)     # Keep columns
data |> select(-state)      # Remove a column
data |> rename(new_name = state)    # Rename a column

# -------------------- FILTER ROWS --------------------

data |> filter(age > 38)

# Comma (AND) — same as using &
data |> filter(state == "CA", income > 30000)
data |> filter(state == "CA" & income > 50000)

# OR — must use | an also use the %in%
data |> filter(state == "CA" | income > 50000)
data |> filter(state %in% c("CA", "OR"))

# Drop the missing values (drops rows containing NA in income)
data |> filter(!is.na(income)) 

# Operators:
# =     Assign or provide an argument
# ==    Test equality
# !=    Not equal
# &     And
# |     Or

# -------------------- CREATE AND MODIFY VARIABLES --------------------

data |> 
  mutate(
    income_thousands = income / 1000
  )

data |> 
  mutate(
    category = case_when(
      score >= 90 ~ "A",
      score >= 80 ~ "B",
      score >= 70 ~ "C",
      TRUE ~ "Below C"
    )
  )

# -------------------- SORT, MISSING VALUES, DUPLICATES --------------------

data |> arrange(age)                  # Ascending order
data |> arrange(desc(age))            # Descending order
data |> drop_na(income)               # Remove rows missing income
data |> replace_na(list(income = 0))  # Replace missing income
data |> distinct()                    # Remove duplicate rows
data |> distinct(name)                  # Returns unique id, no other columns
data |> distinct(name, .keep_all = TRUE)# Returns the first row for each id

# -------------------- RESHAPE DATA --------------------
# Transpose or Pivot in excel
# Wide format: one row per student, separate columns for exams
# This is a summarized table, not usually how data comes in
# student | exam_1 | exam_2 | exam_3

scores_wide_summary <- tibble(
  name = c("Alice", "john", "Carol"),
  exam1 = c(85, 90, 78),
  exam2 = c(92, 88, 95),
  exam3 = c(79, 84, 91)
)
 
# Wide (summarized) -> Long (Raw data)
# Long is more like data you would actually see
scores_long_raw <- scores_wide_summary |> 
  pivot_longer(
    cols = starts_with("exam"),
    names_to = "exam",
    values_to = "score"
  )

scores_long_raw
scores_wide_summary

# Long format is often easier to summarize and visualize
scores_long_raw |> 
  group_by(exam) |> 
  summarise(
    average_score = mean(score, na.rm = TRUE)
  )

# Long -> Wide
# This is more common
scores_wide_summary2 <- scores_long_raw |> 
  pivot_wider(
    names_from = exam,
    values_from = score
  )


# -------------------- COMBINE DATA WITH JOINS --------------------

# A key is a variable used to match rows between tables.

# Join types:
# left_join()  - keeps every row from the left table
# right_join() - keeps every row from the right table
# inner_join() - keeps only matching rows
# full_join()  - keeps every row from both tables

students <- tibble(
  name    = c("Alice", "Bob", "John", "Maria", "Sara"),
  major   = c("Biology", "Computer Science", "Mathematics", "Economics", "Statistics"),
  pincode = c("95112", "94107", "95008", "94103", "95128")
)

students
scores_wide_summary
scores_long_raw

students |> 
  left_join(scores_long_raw, by = "name")


students |> 
  inner_join(scores_wide_summary, by = "name")

students |> 
  right_join(scores_long_raw, by = "name")

students |> 
  full_join(scores_long_raw, by = "name")

# If key names differ:
scores_by_id <- scores_long_raw |> 
  rename(id = name)

students |> 
  left_join(
    scores_by_id,
    by = c("name" = "id")
  )

# -------------------- CHECK UNMATCHED ROWS --------------------

# Students with no matching score
students |> 
  anti_join(scores_long_raw, by = "name")

# Scores with no matching student
scores_long_raw |> 
  anti_join(students, by = "name")

# -------------------- VERIFY A JOIN --------------------

joined <- students |> 
  left_join(scores_long_raw, by = "name")

nrow(scores_long_raw)
nrow(joined)

joined |> 
  summarise(
    rows = n(),
    missing_scores = sum(is.na(score))
  )

# Check for duplicate keys
scores_long_raw |> 
  count(name) |> 
  filter(n > 1)

# GOOD PRACTICE:
# 1. Inspect data before changing it.
# 2. Check missing values and duplicate keys.
# 3. Verify row counts after joins.
# 4. Use clear variable names.
# 5. Explain your cleaning and joining decisions.
