library(FSA)
library(psych)

# Function to fit logistic regression models
fit_logistic_model <- function(data, predictor) {
  model <- glm(data$Is_GAD ~ data[[predictor]], family = binomial(link = "logit"), data = data)
  summary <- summary(model)
  odds_ratios <- exp(coef(model))
  confidence_intervals <- exp(cbind(coef(model), confint(model)))
  
  return(list(
    summary = summary,
    odds_ratios = odds_ratios,
    confidence_intervals = confidence_intervals
  ))
}

# Function to fit models for all predictors
fit_all_models <- function(data) {
  predictors <- c('w1', 'w2', 'w3', 'w4', 'w5', 'w6', 'w7', 'w8')
  models <- lapply(predictors, function(predictor) {
    fit_logistic_model(data, predictor)
  })
  names(models) <- predictors
  return(models)
}

# Load the data
data <- read.csv("~/Downloads/Data-RK.csv")

# Fit models for all predictors
models <- fit_all_models(data)

# Print the model summaries
for (predictor in names(models)) {
  cat("Model for", predictor, ":\n")
  print(models[[predictor]]$summary)
  cat("\nOdds Ratios:\n")
  print(models[[predictor]]$odds_ratios)
  cat("\nConfidence Intervals:\n")
  print(models[[predictor]]$confidence_intervals)
  cat("\n\n")
}

# Subset data for students
students <- data[data$Student == 1, ]

# Fit models for all predictors for students
student_models <- fit_all_models(students)

# Print the model summaries for students
for (predictor in names(student_models)) {
  cat("Model for students -", predictor, ":\n")
  print(student_models[[predictor]]$summary)
  cat("\nOdds Ratios:\n")
  print(student_models[[predictor]]$odds_ratios)
  cat("\nConfidence Intervals:\n")
  print(student_models[[predictor]]$confidence_intervals)
  cat("\n\n")
}
