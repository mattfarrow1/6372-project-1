# From Intro to Statistical Learning 6.5

# Setup -------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(leaps)
library(car)

options(scipen = 999)

# Load data
df <- read_csv(here::here("data - raw", "Life Expectancy Data.csv"))

# Clean up column names
df_clean <- janitor::clean_names(df)

# Data Cleanup ------------------------------------------------------------

# Convert country and status to factors
df_clean$country <- as_factor(df_clean$country)
df_clean$status  <- as_factor(df_clean$status)

# Drop country and year
df_clean <- df_clean %>% 
  select(-country, -year)

# Define leakage columns
leakage <- c("income_composition_of_resources", 
             "percentage_expenditure", 
             "infant_deaths", 
             "under_five_deaths", 
             "adult_mortality")

# Remove leakage columns
df_clean <- df_clean %>% 
  select(-all_of(leakage))

# Best Subset Selection ---------------------------------------------------

# Remove all rows with an NA
df_clean <- na.omit(df_clean)

# Fit model
regfit_full <- regsubsets(life_expectancy ~ ., df_clean)

# Examine regression summary
reg_summary <- summary(regfit_full)
names(reg_summary)
reg_summary$rsq

# Plot RSS, adjusted $R^2$ $C_p$ and BIC for all models
par(mfrow = c(2, 2))
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
which.max(reg_summary$adjr2)
points(8, reg_summary$adjr2[8], col = "red", cex = 2, pch = 20)
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg_summary$cp)
points(8, reg_summary$cp[8], col = "red", cex = 2, pch = 20)
which.min(reg_summary$bic)
plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(8, reg_summary$bic[8], col = "red", cex = 2, pch = 20)

# Display selected variables for the best model with a given number of predictors
plot(regfit_full, scale = "r2")
plot(regfit_full, scale = "adjr2")
plot(regfit_full, scale = "Cp")
plot(regfit_full, scale = "bic")
coef(regfit_full, 6)

# Forward and Backward Stepwise Selection ---------------------------------

# Forward
regfit_fwd <- regsubsets(life_expectancy ~ ., data = df_clean, method = "forward")
summary(regfit_fwd)

# Backward
regfit_bwd <- regsubsets(life_expectancy ~ ., data = df_clean, method = "backward")
summary(regfit_bwd)

# Compare coefficients
coef(regfit_full, 8)
coef(regfit_fwd, 8)
coef(regfit_bwd, 8)

# Choosing Among Models ---------------------------------------------------

# Set seed
set.seed(1)

# Build test and training data sets
train <- sample(c(TRUE, FALSE), nrow(df_clean), rep = TRUE)
test <- (!train)
regfit_best <- regsubsets(life_expectancy ~ ., data = df_clean[train, ])
test_mat <- model.matrix(life_expectancy ~ ., data = df_clean[test, ])

# Run a loop, and for each size `i`, extract the coefficients from `regfit_best`
# for the best model of that size, multiply them into the appropriate columns of
# the test model matric to form the predictions, and compute the test MSE.
val_errors <- rep(NA, 8)
for (i in 1:8) {
  coefi <- coef(regfit_best, id = i)
  pred <- test_mat[, names(coefi)] %*% coefi
  val_errors[i] <- mean((df_clean$life_expectancy[test] - pred)^2)
}

# Find the best model
val_errors
which.min(val_errors)
coef(regfit_best, 8)

# Write a prediction fuction
predict_regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

# Perform best subset selection on the full data set, and select the best model.
regfit_best <- regsubsets(life_expectancy ~ ., data = df_clean)
coef(regfit_best, 8)

# Use cross-validation to choose among models
k <- 10
set.seed(1)
folds <- sample(1:k, nrow(df_clean), replace = TRUE)
cv_errors <- matrix(NA, k, 8, dimnames = list(NULL, paste(1:8)))

# predict.regsubsets = function(object, newdata, id, ...) {
#   form = as.formula(object$call[[2]])
#   mat = model.matrix(form, newdata)
#   coefi = coef(object, id = id)
#   mat[, names(coefi)] %*% coefi
# }

# Perform cross-validation
for (j in 1:k) {
  best_fit <- regsubsets(life_expectancy ~ ., data = df_clean[folds != j, ])
  for (i in 1:8) {
    pred <- predict(best_fit, df_clean[folds == j, ], id = i)
    cv_errors[j, i] <- mean((df_clean$life_expectancy[folds == j] - pred)^2)
  }
}

# Use the apply function to average over the columns of the matrix in order to
# obtain a vector for which the jth element is the cross-validation error for
# the j-variable model.
mean_cv_errors <- apply(cv_errors, 2, mean)
mean_cv_errors
par(mfrow = c(1, 1))
plot(mean_cv_errors, type = "b")

# Perform best subset selection on the full data set in order to obtain the 8-variable model
reg_best <- regsubsets(life_expectancy ~ ., data = df_clean)
best_model <- tidy(coef(reg_best, 8))
