# Load required packages
library(e1071)
library(pso)
library(readxl)

# Load the dataset
data <- read_xlsx("E:/PSO/Appl1.xlsx")

# Split data into features (X) and target variable (y)
X <- data[, c("open", "high", "low", "volume", "daily_return_in_perc", "rsi", "obv")]
y <- data$close

# Scale the features (optional but recommended)
X <- scale(X)

# Define the fitness function for PSO
fitness_function <- function(params, X, y) {
  C <- params[1]
  epsilon <- params[2]
  gamma <- params[3]
  
  # Train the SVR model with the given hyperparameters
  svr_model <- svm(y ~ ., data = data.frame(X, y), kernel = "radial",
                   type = "eps-regression", cost = C, epsilon = epsilon, gamma = gamma)
  
  # Make predictions on the entire dataset
  y_pred <- predict(svr_model, X)
  
  # Calculate the RMSE as the fitness value
  rmse <- sqrt(mean((y_pred - y)^2))
  return(rmse)
}

# Create a dummy function to resolve the 'fn' issue
fn <- function(par, ...) fitness_function(par, X, y)

# Create an empty list to store the results
results <- list()

# Run with a smaller population size (10)
results1 <- psoptim(rep(NA, 3), fn, lower = c(1e-3, 1e-3, 1e-3), upper = c(1e3, 1, 1),
                    control = list(trace = TRUE, maxit = 20, REPORT = 1, s = 10),
                    K = 3, p = 0.271, w0 = 0.7213, w1 = 0.7213, c.p = 1.193, c.g = 1.193,
                    v.max = NA, d = 1000, vectorize = FALSE, hybrid = FALSE)

# Run with default population size (15)
results2 <- psoptim(rep(NA, 3), fn, lower = c(1e-3, 1e-3, 1e-3), upper = c(1e3, 1, 1),
                    control = list(trace = TRUE, maxit = 20, REPORT = 1),
                    K = 3, p = 0.25, w0 = 0.9, w1 = 0.4, c.p = 2.05, c.g = 2.05,
                    v.max = NA, d = 1000, vectorize = FALSE, hybrid = FALSE)

# Run with a larger population size (30)
results3 <- psoptim(rep(NA, 3), fn, lower = c(1e-3, 1e-3, 1e-3), upper = c(1e3, 1, 1),
                    control = list(trace = TRUE, maxit = 20, REPORT = 1, s = 30),
                    K = 3, p = 0.1, w0 = 0.7, w1 = 0.4, c.p = 1.5, c.g = 1.5,
                    v.max = NA, d = 1000, vectorize = FALSE, hybrid = FALSE)
