
library(tidyverse, warn.conflicts = FALSE)
library(broom)
library(knitr)
library(titanic)

data(titanic_train)

titanic_raw <- titanic_train


# ------------------------------------------------------------
# Split the data into training and testing groups
# ------------------------------------------------------------
set.seed(167)

test_rows <- sample(
  seq_len(nrow(titanic_train)),
  size = 0.20 * nrow(titanic_train)
)

test_data <- titanic_train[test_rows, ]

train_data <- titanic_train[-test_rows, ]


##########
# Look at the names

names(train_data)

# [1] "PassengerId" "Survived"    "Pclass"      "Name"        "Sex"         "Age"        
# [7] "SibSp"       "Parch"       "Ticket"      "Fare"        "Cabin"       "Embarked"   

###################
# single variable analysis
train_data |>
  group_by(Parch) |>
  summarize(
    total = n(),
    survived = sum(SibSp),
    died = sum(Survived == 0),
    survival_rate = mean(Survived)
  ) |>
  arrange(Parch) |> print(n=200)

################
# build a women and children first variable
train_data |>
  group_by(Age, Sex) |>
  summarize(
    total = n(),
    survived = sum(Survived),
    died = sum(Survived == 0),
    survival_rate = mean(Survived)
  ) |>
  arrange(desc(survival_rate)) |> arrange(Sex, Age) |>print(n=290)

####################################
# Graph to show age groups by sex
# Create age groups, including missing age as its own group
age_breaks <- c(seq(0, 80, by = 10), Inf)

age_labels <- levels(cut(train_data$Age,breaks = age_breaks,right = FALSE))

age_levels <- c( "Age missing",age_labels)

age_sex_summary <- train_data |>
  mutate(
    age_group = as.character( cut(Age,breaks = age_breaks,right = FALSE )),
    age_group = if_else(is.na(age_group),"Age missing", age_group),
    age_group = factor(age_group,levels = age_levels)
  ) |>
  group_by(Sex, age_group) |>
  summarize(total = n(),survived = sum(Survived),    survival_rate = mean(Survived),
            .groups = "drop"
  )

# Overall survival rate for each sex
sex_survival_rates <- train_data |>
  group_by(Sex) |>
  summarize(overall_survival_rate = mean(Survived, na.rm = TRUE),.groups = "drop")

# Overall null survival rate
null_survival_rate <- mean(  train_data$Survived,  na.rm = TRUE)

ggplot(
  age_sex_summary,
  aes(    x = age_group,    y = survival_rate,    color = Sex,    group = Sex  )
) +
  geom_line() +
  geom_point() +
  
  # Overall male and female survival rates
  geom_hline(
    data = sex_survival_rates,
    aes(      yintercept = overall_survival_rate,      color = Sex    ),
    linetype = "dashed",    inherit.aes = FALSE
  ) +
  # Overall null survival rate
  geom_hline(    yintercept = null_survival_rate,
                 color = "black",    linetype = "dotted"
  ) +
  scale_y_continuous(    labels = scales::percent  ) +
  labs(
    title = "Titanic Survival Rates by Age and Sex",
    subtitle = paste(
      "Dashed lines show overall survival rates by sex.",
      "Dotted line shows the overall null survival rate."
    ),
    x = "Age group",    y = "Survival rate",    color = "Sex"
  ) +
  theme_minimal() +
  theme(    axis.text.x = element_text(angle = 45,hjust = 1    )  )
#########################
# Smooth graph

ggplot(
  train_data |> filter(!is.na(Age)),
  aes(    x = Age,    y = Survived,    color = Sex  )) +
  geom_jitter(    height = 0.05,    width = 0,    alpha = 0.25  ) +
  geom_smooth(    method = "loess",    se = FALSE  ) +
  scale_y_continuous(    breaks = c(0, 1),
                         labels = c("Did not survive", "Survived")  ) +
  labs(    title = "Survival Probability by Age and Sex",
           x = "Age",    y = "Survival outcome",    color = "Sex"  ) +
  theme_minimal()
######################
library(dplyr)

train_data |>
  group_by(Sex) |>
  summarize(
    total = n(),
    
    mean_age = mean(Age, na.rm = TRUE),
    median_age = median(Age, na.rm = TRUE),
    
    survival_rate = mean(Survived, na.rm = TRUE)
  )


#######################
# creating new variables 
titanic_clean <- train_data |>
  mutate(
    family_size = SibSp + Parch + 1,
    is_alone = if_else(family_size == 1, "Alone", "With family"),
    age_missing = if_else(is.na(Age), "Age missing", "Age recorded"),
    new_age = if_else(is.na(Age), 28, Age),
    title = str_extract(Name, "(?<=, )[^.]+"),
    Embarked = replace_na(Embarked, "Unknown"),
    is_1st_class = if_else(Pclass == 1, 1,0),
    is_2nd_class = if_else(Pclass == 2, 1,0),
    women_child_1st = if_else(Sex == 'male' & new_age > 10  , 0,1 )
  ) |>
  mutate(
    title = case_when(
      title %in% c("Mlle", "Ms") ~ "Miss",
      title == "Mme" ~ "Mrs",
#      title %in% c(
#        "Dr", "Rev", "Col", "Major", "Capt", "Sir", "Jonkheer",
#        "Don", "Lady", "Countess"
#      ) ~ "Rare",
      TRUE ~ title
    ),
#    title = fct_lump_min(as.factor(title), min = 10, other_level = "Rare")
  ) |>
  filter(
    Survived %in% c(0, 1),
    Pclass %in% c(1, 2, 3),
    SibSp >= 0,
    Parch >= 0
  )

names(titanic_clean)

titanic_clean |>
  group_by(title) |>
  summarize(
    total = n(),
    survived = sum(Survived),
    died = sum(Survived == 0),
    survival_rate = mean(Survived)
  ) |>
  arrange(desc(title))

##################################
# build full model

names(titanic_clean)

#[1] "PassengerId"     "Survived"        "Pclass"          "Name"            "Sex"            
#[6] "Age"             "SibSp"           "Parch"           "Ticket"          "Fare"           
#[11] "Cabin"           "Embarked"        "family_size"     "is_alone"        "age_missing"    
#[16] "title"           "is_1st_class"    "is_2nd_class"    "women_child_1st"


#######################
# are the dependent variables already related to each other?
prop.table(
  table(titanic_clean$Pclass, titanic_clean$Sex),
  margin = 1
)

###################
# make a model

full_model <- glm(
#  Survived ~ Pclass + Sex +  SibSp + Parch + Embarked + family_size + is_alone + age_missing + title + is_1st_class + is_2nd_class + women_child_1st + new_age,
  Survived ~  Pclass + Sex +  Embarked + family_size + is_alone + age_missing + women_child_1st + new_age,
  #  some variables are too closely related to each other or are doing 
  data = titanic_clean,
  family = binomial
)
summary(full_model)


##############################
# score the test group

score_model <- function(model, data, cutoff = 0.50) {
  
  scored_data <- data |>
    mutate(
      family_size = SibSp + Parch + 1,
      is_alone = if_else(family_size == 1, "Alone", "With family"),
      age_missing = if_else(is.na(Age), "Age missing", "Age recorded"),
      new_age = if_else(is.na(Age), 28, Age),
      title = str_extract(Name, "(?<=, )[^.]+"),
      Embarked = replace_na(Embarked, "Unknown"),
      is_1st_class = if_else(Pclass == 1, 1, 0),
      is_2nd_class = if_else(Pclass == 2, 1, 0),
      women_child_1st = if_else(
        Sex == "male" & new_age > 10,
        0,
        1
      )
    ) |>
    mutate(
      title = case_when(
        title %in% c("Mlle", "Ms") ~ "Miss",
        title == "Mme" ~ "Mrs",
        TRUE ~ title
      )
    ) |>
    mutate(
      predicted_probability = predict(
        model,
        newdata = pick(everything()),
        type = "response"
      ),
      predicted_class = if_else(
        predicted_probability >= cutoff,
        1L,
        0L
      )
    )
  
  return(scored_data)
}

test_predictions <- score_model(model = full_model,data = test_data,cutoff = 0.50)

### confusion_matrix
test_predictions |>
  with(table(Actual = Survived,Predicted = predicted_class))

#########################
### Explain confusion matrix
##########################

roc50 <-test_predictions |> arrange(predicted_probability) |>  
  mutate(cumulative_survivors = cumsum(Survived)) |> 
    select(c(Sex , SibSp , Parch , Embarked , family_size , 
           is_alone , age_missing , is_1st_class , is_2nd_class , 
           women_child_1st , new_age, cumulative_survivors, Survived, 
           predicted_probability, predicted_class)) |>
  mutate(tp = if_else( Survived == 1 & predicted_class == 1,1,0),
         tn = if_else( Survived == 0 & predicted_class == 0,1,0),
         fp = if_else( Survived == 0 & predicted_class == 1,1,0),
         fn = if_else( Survived == 1 & predicted_class == 0,1,0))


###############################
# Let's try different cutoff points, which one is the best?
# ROC curve

scored_data <- test_predictions |>  filter(!is.na(predicted_class))

true_positive  <- sum(scored_data$Survived == 1 & scored_data$predicted_class == 1)
true_negative  <- sum(scored_data$Survived == 0 & scored_data$predicted_class == 0)
false_positive <- sum(scored_data$Survived == 0 & scored_data$predicted_class == 1)
false_negative <- sum(scored_data$Survived == 1 & scored_data$predicted_class == 0)

model_metrics <- tibble(
  true_positive = true_positive,
  true_negative = true_negative,
  false_positive = false_positive,
  false_negative = false_negative,
  accuracy = (true_positive + true_negative) / nrow(scored_data),
  sensitivity = true_positive / (true_positive + false_negative),
  specificity = true_negative / (true_negative + false_positive),
  precision = true_positive / (true_positive + false_positive),
  false_positive_rate = false_positive /
    (false_positive + true_negative),
  false_negative_rate = false_negative /
    (false_negative + true_positive)
)

model_metrics


library(tidyverse)

cutoffs <- seq(0, 1, by = 0.05)

cutoff_rates <- map_dfr(cutoffs, function(current_cutoff) {
  
  scored_data <- score_model(
    model = full_model,
    data = test_data,
    cutoff = current_cutoff
  ) |>
    filter(
      !is.na(Survived),
      !is.na(predicted_class)
    )
  
  true_positive <- sum(
    scored_data$Survived == 1 &
      scored_data$predicted_class == 1
  )
  
  true_negative <- sum(
    scored_data$Survived == 0 &
      scored_data$predicted_class == 0
  )
  
  false_positive <- sum(
    scored_data$Survived == 0 &
      scored_data$predicted_class == 1
  )
  
  false_negative <- sum(
    scored_data$Survived == 1 &
      scored_data$predicted_class == 0
  )
  
  tibble(
    cutoff = current_cutoff,
    true_positive_rate =
      true_positive / (true_positive + false_negative),
    
    true_negative_rate =
      true_negative / (true_negative + false_positive),
    
    accuracy =
      (true_positive + true_negative) /
      (true_positive + true_negative +
         false_positive + false_negative),
    
    precision = if_else(
      true_positive + false_positive > 0,
      true_positive / (true_positive + false_positive),
      NA_real_
    ),
    
    youden_index = true_positive_rate + true_negative_rate - 1
    )
})

cutoff_results

cutoff_rates_long <- cutoff_rates |>
  pivot_longer(
    cols = c(
      true_positive_rate,
      true_negative_rate,
      accuracy,
      precision,
      youden_index
    ),
    names_to = "metric",
    values_to = "rate"
  )

ggplot(
  cutoff_rates_long,
  aes(
    x = cutoff,
    y = rate,
    color = metric
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.05),
    labels = scales::percent
  ) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  labs(
    title = "Model Performance Across Probability Cutoffs",
    x = "Probability cutoff",
    y = "Rate",
    color = "Metric"
  ) +
  theme_minimal()










parsimony_model <-glm(
  Survived ~ Sex + is_1st_class + is_2nd_class + new_age,
  data = titanic_clean,
  family = binomial
)
summary(parsimony_model)

test_predictions_parsimony <- score_model(
  model = parsimony_model,
  data = test_data
)

  