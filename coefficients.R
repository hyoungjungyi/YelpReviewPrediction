library(RSQLite)
setwd("/Users/hyeongjeongyi/Documents/RA")
source("EDA.R")
print(ncol(sample_test_data))
source("modeling.R")
# Initialize lists to store column names
positive_coefficients <- list()
negative_coefficients <- list()

# Extract columns starting with 'count_' for the remaining data
for (i in 1:3) {
  data_name <- paste0("sample_data_", i)
  
  count_columns <- grep("^count_", colnames(get(data_name)), value = TRUE)
  
  if (length(count_columns) > 0) {
    # Select variables for model building
    numeric_columns <- c("rating", count_columns)
    
    # Extract training data
    train_x <- as.matrix(get(data_name)[, numeric_columns, drop = FALSE])
    train_y <- get(data_name)$rating
    
    # Fit Lasso model
    model <- cv.glmnet(x = train_x, y = train_y, alpha = 1)  # alpha = 1 for Lasso
    
    # Extract coefficient values
    coef_values <- as.matrix(coef(model))
    
    # Identify positive and negative coefficients
    positive_indices <- which(coef_values > 0)
    negative_indices <- which(coef_values < 0)
    
    # Extract column names from original data
    coef_names <- colnames(train_x)[-1]  # Exclude 'rating'
    
    # Identify positive and negative coefficient names
    positive_names <- paste("count_", gsub("^count_", "", coef_names[positive_indices]), sep = "")
    negative_names <- paste("count_", gsub("^count_", "", coef_names[negative_indices]), sep = "")
    
    # Store positive and negative coefficient names
    positive_coefficients[[i]] <- c("rating", positive_names)
    negative_coefficients[[i]] <- c("rating", negative_names)
  } else {
    warning(sprintf("No columns starting with 'count_' found in %s.", data_name))
  }
}

print(positive_coefficients)
