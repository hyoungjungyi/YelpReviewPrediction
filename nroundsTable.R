library(RSQLite)
setwd("/Users/hyeongjeongyi/Documents/RA")
source("EDA.R")
print(ncol(sample_test_data))
source("modeling.R")
source("mseTable.R")
# XGBoost
library(xgboost)

params_base <- list(
  objective = "reg:squarederror",  # for regression problems
  max_depth = 3,
  eta = 0.1,
  subsample = 0.8, 
  colsample_bytree = 0.8,  
  colsample_bylevel = 0.8,  
  colsample_bynode = 0.8
)

# Different nrounds values
nrounds_values <- c(295, 300, 310)

# List to store MSE values for each nrounds
mse_values_list <- list()

# Loop through different nrounds values
for (nrounds_val in nrounds_values) {
  params <- params_base
  params$nrounds <- nrounds_val
  
  mse_values <- numeric(3) # List to store MSE for each sample_data
  
  for (i in 1:3) {
    data_name <- paste0("sample_data_", i)
    
    train_data <- get(data_name)[, 2:398]
    labels <- get(data_name)$rating
    train_matrix <- as.matrix(train_data)
    
    model <- xgboost(params = params, data = train_matrix, label = labels, nrounds = params$nrounds)
    
    test_matrix <- as.matrix(sample_test_data[, 2:398])
    predictions <- predict(model, newdata = test_matrix)
    
    mse <- mean((predictions - sample_test_data$rating)^2)
    mse_values[i] <- mse
  }
  
  mse_values_list[[as.character(nrounds_val)]] <- mse_values
}

# Plotting
par(mfrow = c(1, 3), mar = c(5, 4, 4, 2) + 0.1) # Set up a 1x3 plotting grid
my_colors <- c("red", "blue", "green")
sample_size<-c("80","100","120")
for (i in 1:length(nrounds_values)) {
  nrounds_val <- nrounds_values[i]
  mse_values <- mse_values_list[[as.character(nrounds_val)]]
  plot(sample_size,mse_values, type = "b", col = my_colors[i], ylim = c(0, max(unlist(mse_values_list))), 
       main = paste("MSE for nrounds =", nrounds_val), xlab = "Sample Data", ylab = "MSE")
}










#optimal nrounds for diff sample sizes

# Function to train XGBoost model and return MSE
train_xgboost <- function(train_data, labels, params, nrounds) {
  dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = labels)
  model <- xgboost(params = params, data = dtrain, nrounds = nrounds)
  return(model)
}

# Function to calculate MSE
calculate_mse <- function(model, test_data, test_labels) {
  dtest <- xgb.DMatrix(data = as.matrix(test_data))
  predictions <- predict(model, newdata = dtest)
  mse <- mean((predictions - test_labels)^2)
  return(mse)
}

# Initialize parameters
params <- list(
  objective = "reg:squarederror",
  max_depth = 3,
  eta = 0.1,
  subsample = 0.8, 
  colsample_bytree = 0.8,  
  colsample_bylevel = 0.8,  
  colsample_bynode = 0.8
)
  # Initialize empty list to store MSE for each sample
  xgboost_mse <- list()

# Loop through each sample data
for (i in 1:3) {
  data_name <- paste0("sample_data_", i)
  
  train_data <- get(data_name)[, 2:398]
  train_labels <- get(data_name)$rating
  
  # Initialize empty list to store MSE for each nround
  mse_values <- c()
  
  # Loop through different values of nround
  for (nround in seq(50, 200, by = 10)) {
    model <- train_xgboost(train_data, train_labels, params, nround)
    mse <- calculate_mse(model, sample_test_data[, 2:398], sample_test_data$rating)
    mse_values <- c(mse_values, mse)
  }
  
  # Store MSE values for the current sample
  xgboost_mse[[i]] <- mse_values
}

# Plotting
par(mfrow=c(1,3))
for (i in 1:3) {
  plot(seq(50, 200, by = 10), xgboost_mse[[i]], type = "l", 
       xlab = "nround", ylab = "MSE",
       main = paste("Sample", i))
}

