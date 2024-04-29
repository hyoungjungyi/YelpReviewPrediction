# Yelp Rating Prediction Model
## Description
This vignette illustrates an exploratory data analysis (EDA + modeling) performed on Yelp restaurant reviews. The primary goal is to understand the sentiments conveyed through reviews and predict the rating by analyzing the frequency and context of words used.

# Environment Setup and Data Loading

1. Libraries: We start by loading necessary R packages:  
‘RSQLite’ for database interaction.  
‘stringr’ for string manipulation.  
‘tm' for text mining to handle stop words.  
2. Database Connection:  
Set the working directory to where the database file resides.  
Establish a connection to the yelpResData.db SQLite database.  
Read and list tables from the database to ensure it includes a review table.  
```R
library(RSQLite)

setwd("/Users/hyeongjeongyi/Documents/RA")
filename <- "yelpResData.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver, dbname = filename)
dbListTables(db)
```

# Data Preparation
1. Read Data:  
   -Fetch data from the 'review' table which includes review content and ratings  
   -Subset the data to focus on these attributes  
2. Create Training and Test Sets:
   -The first 630,000 rows are designated as the training data.
   -The remaining 158,471 rows are used for testing.
```R
review <- dbReadTable(db, "review")
review_subset <- review[, c('reviewContent', 'rating')]
train_data <- review_subset[1:630000, ]
test_data <- tail(review_subset, n = 158471)

```
# Sampling
Generate Samples:  
  Create smaller samples from the training data for manageable processing and experimentation.
```R
sample_data_1 <- train_data[1:800, ]
sample_data_2 <- train_data[1:1000, ]
sample_data_3 <- train_data[1:1200, ]
```

# DataLoader
The data_loader function is designed to load and configure the initial dataset from a database.
## word_Count
Input review and output word count
```R
text_data<-train_data$reviewContent
library(stringr)
extract_words<-function(sentence){
  unlist(str_extract_all(sentence,"\\b\\w+\\b"))
}
all_words<-unlist(lapply(text_data,extract_words))

#print word frequency
word_freq<-table(all_words) 
word_freq_df<-as.data.frame(word_freq)
top_words <- head(word_freq_df[order(-word_freq_df$Freq),],500)

#get rid of unnecesssary words
library(tm)
stop_words<-stopwords("english")
selectedwords<-setdiff(top_words$all_words,stop_words)

##count the relevant_words in each row
count_word_occurrences <- function(row, word) {
  sum(str_count(row, word))
}

# Count the relevant_words in each row


for (word in selectedwords) {
  counts <- sapply(sample_test_data$reviewContent, count_word_occurrences, word)
  sample_test_data[[paste0("count_", word)]] <- counts
}


for (i in 1:3) {
  data_name <- paste0("sample_data_", i)
  sample_data <- get(data_name)
  for (word in selectedwords) {
    counts <- sapply(sample_data$reviewContent, count_word_occurrences, word)
    sample_data[[paste0("count_", word)]] <- counts
  }
  assign(data_name, sample_data, envir = .GlobalEnv)
}
```
## print_n_reviews
Inputs an integer(k) and prints k amount of randomly selected rows in the sample data
```R
select_random_rows <- function(data, k) {
  set.seed(123)  
  random_rows <- data[sample_test_data(nrow(data), k, replace = FALSE), ]
  return(random_rows)
}

# Prompt user input for the number of rows to select randomly
k <- as.integer(readline(prompt = "Enter the number of rows to select randomly: "))

# Select k random rows using the defined function
selected_rows <- select_random_rows(review, k)

# Print the randomly selected rows
print(selected_rows)
```

# DataProcessor
The data_processor function prepares and transforms the data for analysis.Input review and output the predicted rating using lasso model  
-Arguments:  Linear Regression / Lasso / XGboost  
-Preparation: make lists to store mse values from different models  
```R
lr_mse <- list()
xgboost_mse <- list()
lasso_mse <- list()
```
## Linear Regression
```R
#get rid of Multicollinearity
remove_highly_correlated <- function(data, threshold = 0.8) {
  data <- na.omit(data)
  sd_zero <- sapply(data, sd) == 0
  data <- data[, !sd_zero]
  if(ncol(data) > 1) { 
    correlation_matrix <- cor(data)
    corrplot(correlation_matrix, method = "circle", type = "upper", order = "hclust",
             tl.col = "black", tl.srt = 45, addCoef.col = "black")
    
    high_corr <- which(abs(correlation_matrix) > threshold, arr.ind = TRUE)

    high_corr <- high_corr[high_corr[,1] > high_corr[,2], ]

    to_remove <- c()
    for (i in 1:nrow(high_corr)) {
      to_remove <- unique(c(to_remove, colnames(data)[high_corr[i, 1]]))
    }
    data <- data[, !colnames(data) %in% to_remove]
  } else {
    print("Not enough variables to calculate correlations.")
  }
  return(data)
}
for (i in 1:3) {
  data_name <- paste0("sample_data_", i)
  model_name <- paste0("reg_", i)
  model_data <- get(data_name)
  count_columns <- grep("^count_", names(model_data), value = TRUE)
  
  relevant_data <- model_data[, c("rating", count_columns)]
  filtered_data <- remove_highly_correlated(relevant_data)
  formula <- as.formula(paste("rating ~", paste(names(filtered_data)[-1], collapse = " + ")))
  model <- lm(formula, data = filtered_data)
  
  formula <- paste("rating ~", paste(count_columns, collapse = " + "))
  model <- lm(formula, data = model_data)
  assign(model_name, model)
  test_data_modified <- sample_test_data[, c("rating", count_columns)]
  predictions <- predict(model, newdata = test_data_modified)
  mse <- mean((sample_test_data$rating - predictions)^2)
  lr_mse[[i]] <- mse
}
print(lr_mse)
```

## Lasso
```R
for (i in 1:3) {
  data_name <- paste0("sample_data_", i)
  train_data <- get(data_name)
  train_X <- as.matrix(train_data[, grep("^count_", names(train_data))])
  train_Y <- train_data$rating
  cv_lasso_model <- cv.glmnet(train_X, train_Y, alpha = 1)
  test_X <- as.matrix(sample_test_data[, grep("^count_", names(sample_test_data))])
  # Make predictions 
  predictions <- predict(cv_lasso_model, s = "lambda.min", newx = test_X)
  if (is.matrix(predictions)) {
    predictions <- predictions[,1]
  }
  # Calculate MSE 
  mse <- mean((sample_test_data$rating - predictions)^2)
  lasso_mse[[i]] <- mse
}
print(lasso_mse)
```

## XGBoost
```R
test_X <- as.matrix(sample_test_data[, grep("^count_", names(sample_test_data))])
test_Y <- sample_test_data$rating
dtest <- xgb.DMatrix(data = test_X, label = test_Y)

for (i in 1:3) {
  data_name <- paste0("sample_data_", i)
  train_data <- get(data_name)
  train_X <- as.matrix(train_data[, grep("^count_", names(train_data))])
  train_Y <- train_data$rating
  dtrain <- xgb.DMatrix(data = train_X, label = train_Y)
  # Train 
  params <- list(objective = "reg:squarederror", eta = 0.1, max_depth = 6, eval_metric = "rmse")
  xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100)
  # Make predictions 
  predictions <- predict(xgb_model, dtest)
  # Calculate MSE 
  mse <- mean((sample_test_data$rating - predictions)^2)
  xgboost_mse[[i]] <- mse
}
print(xgboost_mse)
```

