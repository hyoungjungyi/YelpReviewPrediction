library(RSQLite)
setwd("/Users/hyeongjeongyi/Documents/RA")
source("EDA.R")
# Initialize lists to store MSE values for each method
lr_mse <- list()
xgboost_mse <- list()
lasso_mse <- list()

# Linear Regression
for (i in 1:3) {
  data_name <- paste0("sample_data_", i)
  model_name <- paste0("reg_", i)
  
  
  # Fit linear regression model
  model <- lm(rating ~ ., data = get(data_name)[, -1])
  assign(model_name, model)
  
  # Make predictions on the test set
  predictions <- predict(model, newdata = sample_test_data)
  
  # Calculate MSE and add it to lr_mse list
  mse <- mean((sample_test_data$rating - predictions)^2)
  lr_mse[[i]] <- mse
}

# XGBoost
library(xgboost)

# Define your parameters
params <- list(
  objective = "reg:squarederror",  # for regression problems
  max_depth = 3,
  eta = 0.1,
  nrounds = 100,
  subsample = 0.8,
  colsample_bytree = 0.8,
  colsample_bylevel = 0.8,
  colsample_bynode = 0.8
)

# Loop over your sample data sets (sample_data_1, sample_data_2, sample_data_3)
for (i in 1:3) {
  data_name <- paste0("sample_data_", i)
  model_name <- paste0("model_", i)
  
  # Extract rating column from sample_data_i
  train_data <- get(data_name)[, which(names(get(data_name)) == "rating")]
  labels <- get(data_name)$rating
  
  # Ensure train_data is numeric
  train_matrix <- as.matrix(train_data)
  
  # Train XGBoost model
  model <- xgboost(params = params, data = train_matrix, label = labels, nrounds = params$nrounds)
  
  # Assign model to model_i
  assign(model_name, model)
  
  # Prepare test data (assuming sample_test_data is defined elsewhere)
  test_features <- sample_test_data[, names(train_matrix)]
  
  # Convert logical columns to numeric
  test_features <- apply(test_features, 2, as.numeric)
  
  # Ensure test_features is numeric
  test_matrix <- as.matrix(test_features)
  
  # Make predictions on test data
  predictions <- predict(model, newdata = xgb.DMatrix(test_matrix))
  
  # Calculate MSE
  mse <- mean((predictions - sample_test_data$rating)^2)
  print(paste("MSE for model", i, ":", mse))
}



# Lasso
library(glmnet)

# Initialize lists
lasso_mse_values <- list()

# Loop
for (i in 1:3) {
  data_name <- paste0("sample_data_", i)
  
  # Extract columns starting with 'count_'
  count_columns <- grep("^count_", colnames(get(data_name)), value = TRUE)
  
  if (length(count_columns) > 0) {
    # Select variables for model building
  
    print("here")
    # Extract training data
    train_x <- as.matrix(get(data_name)[, count_columns, drop = FALSE])
    train_y <- get(data_name)$rating
    
    # Fit Lasso model
    model <- cv.glmnet(x = train_x, y = train_y, alpha = 1)  # alpha = 1 for Lasso
    
    # Model summary
    print(summary(model))
    
    # Load test data and make predictions
    test_data <- sample_test_data
    test_x <- as.matrix(test_data[, count_columns, drop = FALSE])
    test_y <- test_data$rating
    
    predictions <- predict(model, newx = test_x, s = "lambda.min")
    
    # Calculate MSE
    mse <- mean((predictions - test_y)^2)
    lasso_mse_values[[i]] <- mse
  } else {
    warning(sprintf("No columns starting with 'count_' found in %s. Skipping iteration %d.", data_name, i))
  }
}

model$cvm



# Combine MSE values into a data frame
mse_data <- data.frame(
  Method = rep(c("Linear Regression", "XGBoost", "Lasso"), each = 3),
  SubsampleSize = rep(c(800, 1000, 1200), times = 3),
  MSE = c(lr_mse[[1]], lr_mse[[2]], lr_mse[[3]], xgboost_mse[[1]], xgboost_mse[[2]], xgboost_mse[[3]], lasso_mse_values[[1]], lasso_mse_values[[2]], lasso_mse_values[[3]])
)

# Print the resulting data frame
print(mse_data) 


