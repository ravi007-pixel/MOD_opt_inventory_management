## Header: Load required libraries
library(dplyr)  # For data manipulation

## Header: Function to fit linear model
getBenchmark <- function(X, y) {
  # Fit linear model
  mod <- lm(y ~ ., data = data.frame(X))
  
  return(mod)
}

## Header: Function to get data
getData <- function() {
  # Load your dataset
  company_data <- read.csv("E:/moder_opt/clean_big_basket.csv")
  
  # Remove rows with missing values in the target variable
  company_data <- na.omit(company_data, cols = "rating")
  
  # Set the target variable as 'rating'
  target_var <- 'rating'
  
  # Split data into features and target
  X <- data.matrix(company_data[, !names(company_data) %in% target_var])  # Convert to matrix
  y <- company_data[[target_var]]
  
  return(list(X = X, y = y))
}

## Header: Feature Fitness Function for Linear Model
featureFitness <- function(string, X, y) {
  # print(string)  # Uncomment this line if you want to print every single solution
  inc <- which(string == 1)  # 'inc' includes those features/variables for which 'string' contains 1
  if (length(inc) == 0) return(-10E20)  # If no feature is selected, give a terrible fitness to this solution
  
  # Create a data frame of values for all the variables contained in 'inc'
  X_subset <- data.frame(X[, inc, drop = FALSE])  # Convert to data frame
  
  # Fit linear model on the subset of features
  mod <- lm(y ~ ., data = cbind(X_subset, y))
  
  # Calculate the mean squared error (MSE) as the fitness metric
  y_pred <- predict(mod, X_subset)
  mse <- mean((y - y_pred)^2)
  
  # Minimize the MSE
  -mse
}