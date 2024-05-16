library(dplyr)

# Function to calculate factor scores
calculate_factors <- function(data) {
  PQ1 <- data$PSS1
  PQ2 <- data$PSS2
  PQ3 <- data$PSS3
  PQ4 <- data$PSS4
  PQ5 <- data$PSS5
  PQ6 <- data$PSS6
  PQ7 <- data$PSS7
  PQ8 <- data$PSS8
  PQ9 <- data$PSS9
  PQ10 <- data$PSS10
  
  df1 <- data.frame(PQ1, PQ2, PQ3, PQ6, PQ9, PQ10)
  Factor1 <- df1 %>%
    replace(is.na(.), 0) %>%
    mutate(factor1 = rowSums(.[1:6]))
  
  df2 <- data.frame(PQ4, PQ5, PQ7, PQ8)
  Factor2 <- df2 %>%
    replace(is.na(.), 0) %>%
    mutate(factor2 = rowSums(.[1:4]))
  
  DFSum <- data.frame(PQ1, PQ2, PQ3, PQ6, PQ9, PQ10, PQ4, PQ5, PQ7, PQ8)
  TotalSum <- DFSum %>%
    replace(is.na(.), 0) %>%
    mutate(sum = rowSums(.[1:10]))
  
  return(TotalSum$sum)
}

# Function to classify stress levels
classify_stress_levels <- function(total_sum) {
  low_stress <- sum(total_sum >= 0 & total_sum < 14)
  moderate_stress <- sum(total_sum >= 14 & total_sum <= 26)
  high_stress <- sum(total_sum >= 27 & total_sum <= 40)
  
  return(list(
    low_stress = low_stress,
    moderate_stress = moderate_stress,
    high_stress = high_stress
  ))
}

# Function to calculate Spearman correlation coefficients
calculate_spearman_correlations <- function(data, total_sum) {
  correlations <- sapply(data, function(pq) {
    cor.test(x = pq, y = total_sum, method = 'spearman', exact = FALSE)$estimate
  })
  return(correlations)
}

# Main function to run the analysis
run_analysis <- function(data) {
  total_sum <- calculate_factors(data)
  stress_levels <- classify_stress_levels(total_sum)
  correlations <- calculate_spearman_correlations(data, total_sum)
  
  return(list(
    total_sum = total_sum,
    stress_levels = stress_levels,
    correlations = correlations
  ))
}

# Load the data
data <- rk_coded_preprocessed_data_final

# Run the analysis
results <- run_analysis(data)

# Print the results
print(results$stress_levels)
print(results$correlations)
