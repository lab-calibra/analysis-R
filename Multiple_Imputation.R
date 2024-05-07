library(sjPlot)
library(sjmisc)
library(ggplot2)
library(dplyr)
library(tidyverse)
theme_set(theme_sjplot())
library(mice)

# Load the data
data <- read.csv(path)

# Auto-convert all categorical variables into factors
binary_columns <- sapply(data, function(x) all(x %in% c(0, 1, 2)))
for (col in names(data)[binary_columns]) {
  data[[col]] <- as.factor(data[[col]])
}

# If using COGNITO as DVs, z-score all the continuous coeffecients
data %>% mutate(across(where(is.integer), scale))

# Check number of missing variables
data %>% summarise_all(~ sum(is.na(.)))

# Now, let us do multiple imputation
# Perform imputation using MICE
# m: Number of Imputations; as a rule of thumb maximum missing percentage
# maxit: Number of Iterations; normally stable results at 10
imp_data <- mice(data = data,
                 m = 10,
                 maxit = 10,
                 seed = 42,
                 print = FALSE)

# Save the imputated data
imp_datasets <- complete(imp_data,"long")

results <-with(imp_data,lm(EP03BR_ReadingSyntaxComprehension ~ Median_Millets + Age + Gender + Education + Smoking_Code + Alcohol_Code + HTN.status + DM.status + GDS_Score + BMI + Physically.active.or.inactive))
results

pool <- pool (results)
summary (pool)

# List of outcome variables
outcome_vars <- c("EP11BR_GeometricFigures",
                  "EP05BRT_VisualAttention",
                  "EP04BRT_AuditoryAttention",
                  "EP03BR_ReadingSyntaxComprehension",
                  "EP17ET01BR_DelayedRecallNames",
                  "HMSE")

# Create an empty data frame to store results
results <- data.frame()

for (var in outcome_vars) {
  
  # OLS formula
  formula = as.formula(paste(var, "~ Median_Millets + Age + Gender + Education + Individual_Annual_Income +Smoking_Code + Alcohol_Code + HTN.status + DM.status + GDS_Score + BMI + Physically.active.or.inactive"))
  model <- lm(formula, data = imp_datasets)
  print(var)
  
  # Save the model summary
  model_summary <- summary(model)
  print(model_summary)
  
  # Round the model coeffs and CI
  model_summary$coefficients <- round(model_summary$coefficients, 2)
  model_summary$r.squared <- round(model_summary$r.squared, 2)
  model_summary$adj.r.squared <- round(model_summary$adj.r.squared, 2)
  coef_ci <- confint(model)
  coef_ci <- round(coef_ci, 2)
  
  # Combine model summary and confidence intervals
  combined_results <- cbind(model_summary$coefficients, model_summary$r.squared, model_summary$adj.r.squared,coef_ci)
  
  # Add a column with the outcome variable name
  combined_results <- cbind(Outcome_Variable = var, combined_results)
  
  # Append to the results data frame
  results <- rbind(results, combined_results)
  
}

# Save the results dataframe to a CSV file
write.csv(results, "OLS_RAW_MultipleImputation.csv", row.names = TRUE)
