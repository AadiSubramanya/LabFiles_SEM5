# 1. LOAD THE REQUIRED PACKAGE
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}

library(readxl)

# 2. IMPORT THE EXCEL DATASET
energy <- read_excel(
  "C:/Users/maths/Documents/Aadi_240957002/MVA/week6/Energy_Efficiency_Multivariate_Regression_data.xlsx"
)

energy <- as.data.frame(energy)

# Display the first six observations
head(energy)

# 3. EXPLORE THE DATASET
str(energy)
dim(energy)
names(energy)
summary(energy)

# 4. CHECK FOR MISSING VALUES
colSums(is.na(energy))

required_variables <- c(
  "Y1_Heating_Load",
  "Y2_Cooling_Load",
  "X1_Relative_Compactness",
  "X2_Surface_Area",
  "X3_Wall_Area",
  "X4_Roof_Area",
  "X5_Overall_Height",
  "X6_Orientation",
  "X7_Glazing_Area"
)

missing_variables <- setdiff(required_variables, names(energy))

if (length(missing_variables) > 0) {
  stop(paste("Missing variables:", paste(missing_variables, collapse = ", ")))
}

energy <- energy[complete.cases(energy[, required_variables]), ]

# 5. CONVERT ORIENTATION INTO A CATEGORICAL VARIABLE
energy$X6_Orientation <- as.factor(energy$X6_Orientation)
levels(energy$X6_Orientation)
table(energy$X6_Orientation)

# 6. DESCRIPTIVE STATISTICS OF THE RESPONSE VARIABLES
summary(energy$Y1_Heating_Load)
summary(energy$Y2_Cooling_Load)

mean(energy$Y1_Heating_Load)
mean(energy$Y2_Cooling_Load)

sd(energy$Y1_Heating_Load)
sd(energy$Y2_Cooling_Load)

# 7. EXPLORE THE RELATIONSHIP BETWEEN THE TWO RESPONSES
cor(energy$Y1_Heating_Load, energy$Y2_Cooling_Load)

plot(
  energy$Y1_Heating_Load,
  energy$Y2_Cooling_Load,
  main = "Heating Load versus Cooling Load",
  xlab = "Heating Load",
  ylab = "Cooling Load",
  pch = 19
)

# 8. FORMULATE THE MULTIVARIATE MULTIPLE REGRESSION MODEL
model <- lm(
  cbind(Y1_Heating_Load, Y2_Cooling_Load) ~
    X1_Relative_Compactness +
    X2_Surface_Area +
    X3_Wall_Area +
    X4_Roof_Area +
    X5_Overall_Height +
    X6_Orientation +
    X7_Glazing_Area,
  data = energy
)

# 9. DISPLAY THE REGRESSION RESULTS
summary(model)

# 10. EXTRACT THE ESTIMATED REGRESSION COEFFICIENTS
coefficients <- coef(model)
print(coefficients)

# 11. WRITE THE FITTED REGRESSION EQUATIONS
print_equation <- function(coef_values, response_name) {
  coef_values <- round(coef_values, 4)
  intercept <- coef_values[1]
  
  if (is.na(intercept) || !is.finite(intercept)) {
    cat("The intercept for", response_name, "cannot be estimated.\n")
    return(invisible(NULL))
  }
  
  predictor_coefs <- coef_values[-1]
  predictor_names <- names(predictor_coefs)
  
  equation <- paste0(response_name, " = ", intercept)
  
  for (i in seq_along(predictor_coefs)) {
    b <- predictor_coefs[i]
    predictor <- predictor_names[i]
    
    if (is.na(b) || !is.finite(b)) {
      next
    }
    
    if (b >= 0) {
      equation <- paste0(equation, " + ", abs(b), " * ", predictor)
    } else {
      equation <- paste0(equation, " - ", abs(b), " * ", predictor)
    }
  }
  
  cat(equation, "\n\n")
}

print_equation(coefficients[, "Y1_Heating_Load"], "Fitted Heating Load")
print_equation(coefficients[, "Y2_Cooling_Load"], "Fitted Cooling Load")

# 12. INTERPRET THE REGRESSION COEFFICIENTS
summary(model)$`Response Y1_Heating_Load`
summary(model)$`Response Y2_Cooling_Load`


# 13. OBTAIN THE FITTED VALUES
fitted_values <- fitted(model)
head(fitted_values)

fitted_heating <- fitted_values[, 1]
fitted_cooling <- fitted_values[, 2]

# 14. OBTAIN THE RESIDUALS
residual_values <- residuals(model)
head(residual_values)

residual_heating <- residual_values[, 1]
residual_cooling <- residual_values[, 2]

# 15. COMPARE OBSERVED AND FITTED HEATING LOADS
heating_results <- data.frame(
  Observed_Heating = energy$Y1_Heating_Load,
  Fitted_Heating = fitted_heating,
  Residual_Heating = residual_heating
)
head(heating_results)

# 16. COMPARE OBSERVED AND FITTED COOLING LOADS
cooling_results <- data.frame(
  Observed_Cooling = energy$Y2_Cooling_Load,
  Fitted_Cooling = fitted_cooling,
  Residual_Cooling = residual_cooling
)
head(cooling_results)

# 17. PLOT OBSERVED VERSUS FITTED HEATING LOADS
plot(
  energy$Y1_Heating_Load,
  fitted_heating,
  main = "Observed versus Fitted Heating Load",
  xlab = "Observed Heating Load",
  ylab = "Fitted Heating Load",
  pch = 19
)
abline(a = 0, b = 1, col = "red", lwd = 2)


# 18. PLOT OBSERVED VERSUS FITTED COOLING LOADS
plot(
  energy$Y2_Cooling_Load,
  fitted_cooling,
  main = "Observed versus Fitted Cooling Load",
  xlab = "Observed Cooling Load",
  ylab = "Fitted Cooling Load",
  pch = 19
)
abline(a = 0, b = 1, col = "red", lwd = 2)


# 19. RESIDUALS VERSUS FITTED HEATING LOADS
plot(
  fitted_heating,
  residual_heating,
  main = "Residuals versus Fitted Heating Load",
  xlab = "Fitted Heating Load",
  ylab = "Residuals",
  pch = 19
)
abline(h = 0, col = "red", lwd = 2)


# 20. RESIDUALS VERSUS FITTED COOLING LOADS
plot(
  fitted_cooling,
  residual_cooling,
  main = "Residuals versus Fitted Cooling Load",
  xlab = "Fitted Cooling Load",
  ylab = "Residuals",
  pch = 19
)
abline(h = 0, col = "red", lwd = 2)


# 21. HISTOGRAMS OF THE RESIDUALS
hist(
  residual_heating,
  main = "Histogram of Heating Load Residuals",
  xlab = "Residuals",
  col = "lightblue"
)

hist(
  residual_cooling,
  main = "Histogram of Cooling Load Residuals",
  xlab = "Residuals",
  col = "lightgreen"
)


# 22. CALCULATE THE MEAN SQUARED ERROR
MSE_heating <- mean(residual_heating^2)
MSE_cooling <- mean(residual_cooling^2)

cat("MSE for Heating Load =", MSE_heating, "\n")
cat("MSE for Cooling Load =", MSE_cooling, "\n")


# 23. CALCULATE R-SQUARED FOR EACH RESPONSE
R2_heating <- 1 - sum(residual_heating^2) / sum((energy$Y1_Heating_Load - mean(energy$Y1_Heating_Load))^2)
R2_cooling <- 1 - sum(residual_cooling^2) / sum((energy$Y2_Cooling_Load - mean(energy$Y2_Cooling_Load))^2)

cat("R-squared for Heating Load =", R2_heating, "\n")
cat("R-squared for Cooling Load =", R2_cooling, "\n")


# 24. SAVE ALL OBSERVED AND FITTED VALUES
results <- data.frame(
  Observed_Heating = energy$Y1_Heating_Load,
  Fitted_Heating = fitted_heating,
  Residual_Heating = residual_heating,
  Observed_Cooling = energy$Y2_Cooling_Load,
  Fitted_Cooling = fitted_cooling,
  Residual_Cooling = residual_cooling
)
head(results)

# ==========================================
# 25. PRACTICE QUESTION[cite: 11]
# ==========================================

# Question:
# Using the Energy Efficiency dataset, fit a multivariate
# multiple regression model with Heating Load and Cooling Load
# as the two response variables and the following predictors:
# - X3_Wall_Area
# - X4_Roof_Area
# - X7_Glazing_Area
#
# Perform the following tasks:
# (a) Fit the multivariate multiple regression model.
# (b) Display the regression summaries for both response variables.


# ==========================================
# SOLUTION FOR PRACTICE QUESTION
# ==========================================

# (a) Fit the multivariate multiple regression model 
# using X3_Wall_Area, X4_Roof_Area, and X7_Glazing_Area as predictors.

practice_model <- lm(
  cbind(Y1_Heating_Load, Y2_Cooling_Load) ~
    X3_Wall_Area +
    X4_Roof_Area +
    X7_Glazing_Area,
  data = energy
)

# (b) Display the regression summaries for both response variables

# Option 1: Display the complete summary for both responses together
summary(practice_model)

# Option 2: Display the separate coefficient summaries for each response 
# (similar to Section 12 of the lab)
summary(practice_model)$`Response Y1_Heating_Load`
summary(practice_model)$`Response Y2_Cooling_Load`











