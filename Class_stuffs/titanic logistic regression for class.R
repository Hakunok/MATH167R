# Titanic logistic regression script
#
# This script contains the executable R code from the Quarto lesson.
# Comments identify the original section and subsection for each block.
# Run the blocks from top to bottom because later sections use objects
# created in earlier sections.

# ------------------------------------------------------------
# Source: 3. The mathematics behind logistic regression > Logistic regression
# ------------------------------------------------------------
eta <- seq(-8, 8, length.out = 400)
p <- 1 / (1 + exp(-eta))

plot(
  eta,
  p,
  type = "l",
  lwd = 3,
  col = "steelblue",
  ylim = c(0, 1),
  xlab = expression(eta == log(p / (1 - p))),
  ylab = "Probability p",
  main = "The Logistic Function"
)
abline(h = c(0, 1), lty = 3, col = "gray60")
abline(v = 0, lty = 3, col = "gray60")

# ------------------------------------------------------------
# Source: 6. Get the data
# ------------------------------------------------------------
# Run this once if the package is not installed:
# install.packages("titanic")

library(tidyverse, warn.conflicts = FALSE)
library(broom)
library(knitr)
library(titanic)

data(titanic_train)

titanic_raw <- titanic_train

# ------------------------------------------------------------
# Source: 6. Get the data
# ------------------------------------------------------------
dim(titanic_raw)
glimpse(titanic_raw)
summary(titanic_raw)

# ------------------------------------------------------------
# Source: 7. Check for errors and missing values
# ------------------------------------------------------------
titanic_raw |>
  count(Survived)

# ------------------------------------------------------------
# Source: 7. Check for errors and missing values
# ------------------------------------------------------------
titanic_raw |>
  summarize(
    min_age = min(Age, na.rm = TRUE),
    max_age = max(Age, na.rm = TRUE),
    min_fare = min(Fare, na.rm = TRUE),
    max_fare = max(Fare, na.rm = TRUE),
    min_sibsp = min(SibSp, na.rm = TRUE),
    min_parch = min(Parch, na.rm = TRUE)
  )

# ------------------------------------------------------------
# Source: 7. Check for errors and missing values
# ------------------------------------------------------------
titanic_raw |>
  summarize(
    across(everything(), ~ sum(is.na(.x)))
  )

# ------------------------------------------------------------
# Source: 8. Create new variables
# ------------------------------------------------------------
titanic_clean <- titanic_raw |>
  mutate(
    family_size = SibSp + Parch + 1,
    is_alone = if_else(family_size == 1, "Alone", "With family"),
    age_missing = if_else(is.na(Age), "Age missing", "Age recorded"),
    title = str_extract(Name, "(?<=, )[^.]+"),
    Embarked = replace_na(Embarked, "Unknown")
  ) |>
  mutate(
    title = case_when(
      title %in% c("Mlle", "Ms") ~ "Miss",
      title == "Mme" ~ "Mrs",
      title %in% c(
        "Dr", "Rev", "Col", "Major", "Capt", "Sir", "Jonkheer",
        "Don", "Lady", "Countess"
      ) ~ "Rare",
      TRUE ~ title
    ),
    title = fct_lump_min(as.factor(title), min = 10, other_level = "Rare")
  ) |>
  filter(
    Survived %in% c(0, 1),
    Pclass %in% c(1, 2, 3),
    Fare >= 0,
    SibSp >= 0,
    Parch >= 0
  )

# ------------------------------------------------------------
# Source: 9. Explore the outcome and predictors
# ------------------------------------------------------------
ggplot(titanic_clean, aes(x = factor(Survived))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Titanic Survival Counts",
    x = "Survived",
    y = "Number of passengers"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# Source: 9. Explore the outcome and predictors
# ------------------------------------------------------------
ggplot(titanic_clean, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Survival Proportion by Sex",
    x = "Sex",
    y = "Proportion",
    fill = "Survived"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# Source: 9. Explore the outcome and predictors > Simple counts and averages
# ------------------------------------------------------------
titanic_raw |>
  group_by(Pclass) |>
  summarize(
    total = n(),
    Survived = sum(Survived),
    died = total - Survived,
    survival_rate = mean(Survived),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Source: 9. Explore the outcome and predictors > Simple counts and averages
# ------------------------------------------------------------
titanic_raw |>
  group_by(Sex) |>
  summarize(
    total = n(),
    Survived = sum(Survived),
    died = total - Survived,
    survival_rate = mean(Survived),
    .groups = "drop"
  )

titanic_raw |>
  group_by(Embarked) |>
  summarize(
    total = n(),
    Survived = sum(Survived),
    died = total - Survived,
    survival_rate = mean(Survived),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Source: 9. Explore the outcome and predictors > Simple counts and averages
# ------------------------------------------------------------
titanic_raw |>
  group_by(Survived) |>
  summarize(
    passengers = n(),
    ages_recorded = sum(!is.na(Age)),
    average_age = mean(Age, na.rm = TRUE),
    median_age = median(Age, na.rm = TRUE),
    average_fare = mean(Fare, na.rm = TRUE),
    median_fare = median(Fare, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Source: 10. Decide what to do with missing ages
# ------------------------------------------------------------
titanic_clean |>
  group_by(age_missing) |>
  summarize(
    passengers = n(),
    survival_rate = mean(Survived),
    average_fare = mean(Fare, na.rm = TRUE),
    average_sibsp = mean(SibSp, na.rm = TRUE),
    percent_alone = mean(is_alone == "Alone") * 100,
    .groups = "drop"
  )

# ------------------------------------------------------------
# Source: 11. Split the data into training and testing groups
# ------------------------------------------------------------
set.seed(167)

test_rows <- sample(
  seq_len(nrow(titanic_clean)),
  size = 0.20 * nrow(titanic_clean)
)

test_data <- titanic_clean[test_rows, ]
train_data <- titanic_clean[-test_rows, ]

train_data <- train_data |>
  mutate(Age_missing = is.na(Age))

test_data <- test_data |>
  mutate(Age_missing = is.na(Age))

training_median_age <- median(train_data$Age, na.rm = TRUE)

train_data <- train_data |>
  mutate(Age = replace_na(Age, training_median_age))

test_data <- test_data |>
  mutate(Age = replace_na(Age, training_median_age))

# ------------------------------------------------------------
# Source: 11. Split the data into training and testing groups > Dummy variables for categorical predictors
# ------------------------------------------------------------
model.matrix(~ Pclass + Sex + Embarked + title, data = train_data) |>
  head()

# ------------------------------------------------------------
# Source: 12. Fit a logistic regression model
# ------------------------------------------------------------
logistic_model <- glm(
  Survived ~ Pclass + Sex + Age + Age_missing + Fare + family_size +
    is_alone + Embarked + title,
  data = train_data,
  family = binomial
)

summary(logistic_model)

# ------------------------------------------------------------
# Source: 12. Fit a logistic regression model
# ------------------------------------------------------------
tidy(logistic_model, conf.int = FALSE) |>
  mutate(
    odds_ratio = exp(estimate),
    conf.low = exp(estimate - 1.96 * std.error),
    conf.high = exp(estimate + 1.96 * std.error)
  ) |>
  select(term, estimate, odds_ratio, conf.low, conf.high, p.value) |>
  kable(digits = 3)

# ------------------------------------------------------------
# Source: 13. Check the fitted model
# ------------------------------------------------------------
tidy(logistic_model) |>
  arrange(desc(abs(estimate))) |>
  kable(digits = 3)

# ------------------------------------------------------------
# Source: 13. Check the fitted model
# ------------------------------------------------------------
train_data |>
  select(Pclass, Age, Fare, family_size, SibSp, Parch) |>
  cor()

# ------------------------------------------------------------
# Source: 13. Check the fitted model > Create a parsimonious final model
# ------------------------------------------------------------
final_model <- glm(
  Survived ~ Pclass + Sex + Age,
  data = train_data,
  family = binomial
)

summary(final_model)

# ------------------------------------------------------------
# Source: 13. Check the fitted model > Create a parsimonious final model
# ------------------------------------------------------------
tidy(final_model, conf.int = FALSE) |>
  mutate(
    odds_ratio = exp(estimate),
    conf.low = exp(estimate - 1.96 * std.error),
    conf.high = exp(estimate + 1.96 * std.error)
  ) |>
  select(term, estimate, odds_ratio, conf.low, conf.high, p.value) |>
  kable(digits = 3)

# ------------------------------------------------------------
# Source: 14. Stepwise logistic regression > Forward selection
# ------------------------------------------------------------
null_model <- glm(
  Survived ~ 1,
  data = train_data,
  family = binomial
)

full_model <- glm(
  Survived ~ Pclass + Sex + Age + Fare + family_size +
    is_alone + Embarked + title,
  data = train_data,
  family = binomial
)

forward_model <- step(
  null_model,
  scope = list(
    lower = formula(null_model),
    upper = formula(full_model)
  ),
  direction = "forward",
  trace = 1
)

summary(forward_model)

# ------------------------------------------------------------
# Source: 14. Stepwise logistic regression > Backward selection
# ------------------------------------------------------------
backward_model <- step(
  full_model,
  direction = "backward",
  trace = 1
)

summary(backward_model)

# ------------------------------------------------------------
# Source: 14. Stepwise logistic regression > Backward selection
# ------------------------------------------------------------
ggplot(train_data, aes(x = Age, y = Survived)) +
  geom_jitter(height = 0.05, alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = FALSE, color = "red") +
  labs(
    title = "Survival and Age",
    x = "Age",
    y = "Survived"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# Source: 16. Predict probabilities for the test data
# ------------------------------------------------------------
test_predictions <- test_data |>
  mutate(
    probability_survived = predict(
      final_model,
      newdata = test_data,
      type = "response"
    ),
    predicted_class = if_else(
      probability_survived >= 0.50,
      1,
      0
    )
  ) |>
  select(
    PassengerId,
    Survived,
    Pclass,
    Sex,
    Age,
    probability_survived,
    predicted_class
  )

head(test_predictions , n = 10)

# ------------------------------------------------------------
# Source: 17. Score the test predictions
# ------------------------------------------------------------
confusion_matrix <- table(
  Actual = test_predictions$Survived,
  Predicted = test_predictions$predicted_class
)

confusion_matrix

# ------------------------------------------------------------
# Source: 17. Score the test predictions
# ------------------------------------------------------------
true_positive <- sum(
  test_predictions$Survived == 1 &
    test_predictions$predicted_class == 1
)

true_negative <- sum(
  test_predictions$Survived == 0 &
    test_predictions$predicted_class == 0
)

false_positive <- sum(
  test_predictions$Survived == 0 &
    test_predictions$predicted_class == 1
)

false_negative <- sum(
  test_predictions$Survived == 1 &
    test_predictions$predicted_class == 0
)

accuracy <- (true_positive + true_negative) / nrow(test_predictions)
sensitivity <- true_positive / (true_positive + false_negative)
specificity <- true_negative / (true_negative + false_positive)
precision <- true_positive / (true_positive + false_positive)

tibble(
  accuracy = accuracy,
  sensitivity = sensitivity,
  specificity = specificity,
  precision = precision
)

# ------------------------------------------------------------
# Source: 17. Score the test predictions > Log loss and the Brier score
# ------------------------------------------------------------
brier_score <- mean(
  (test_predictions$Survived -
     test_predictions$probability_survived)^2
)

brier_score

# ------------------------------------------------------------
# Source: 18. ROC curve and AUC
# ------------------------------------------------------------
roc_data <- map_dfr(
  seq(0, 1, by = 0.01),
  function(cutoff) {
    predicted <- if_else(
      test_predictions$probability_survived >= cutoff,
      1,
      0
    )

    tp <- sum(test_predictions$Survived == 1 & predicted == 1)
    tn <- sum(test_predictions$Survived == 0 & predicted == 0)
    fp <- sum(test_predictions$Survived == 0 & predicted == 1)
    fn <- sum(test_predictions$Survived == 1 & predicted == 0)

    tibble(
      cutoff = cutoff,
      sensitivity = tp / (tp + fn),
      specificity = tn / (tn + fp),
      false_positive_rate = 1 - specificity
    )
  }
)

ggplot(roc_data, aes(x = false_positive_rate, y = sensitivity)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_abline(linetype = "dashed", color = "gray50") +
  coord_equal() +
  labs(
    title = "ROC Curve",
    x = "False-positive rate",
    y = "True-positive rate"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# Source: 18. ROC curve and AUC
# ------------------------------------------------------------
best_cutoff <- roc_data |>
  mutate(youden_index = sensitivity + specificity - 1) |>
  slice_max(youden_index, n = 1, with_ties = FALSE)

best_cutoff

# ------------------------------------------------------------
# Source: 18. ROC curve and AUC
# ------------------------------------------------------------
roc_ordered <- roc_data |>
  arrange(false_positive_rate)

auc <- sum(
  diff(roc_ordered$false_positive_rate) *
    (head(roc_ordered$sensitivity, -1) +
       tail(roc_ordered$sensitivity, -1)) / 2
)

auc

# ------------------------------------------------------------
# Source: 19. Cumulative gains chart
# ------------------------------------------------------------
gains_data <- test_predictions |>
  arrange(desc(probability_survived)) |>
  mutate(
    rank = row_number(),
    population_percent = rank / n() * 100,
    cumulative_survivors = cumsum(Survived),
    gains_percent = cumulative_survivors / sum(Survived) * 100,
    random_gains_percent = population_percent
  )

# ------------------------------------------------------------
# Source: 19. Cumulative gains chart
# ------------------------------------------------------------
ggplot(gains_data, aes(x = population_percent, y = gains_percent)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_line(
    aes(y = random_gains_percent),
    color = "gray50",
    linetype = "dashed"
  ) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(
    title = "Cumulative Gains Chart",
    subtitle = "How many survivors are captured by investigating high-probability cases first",
    x = "Percentage of passengers considered",
    y = "Percentage of survivors captured"
  ) +
  theme_minimal()
