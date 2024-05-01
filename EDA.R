library(RSQLite)
setwd("/Users/hyeongjeongyi/Documents/RA")
library(NLP)
#opening the file
filename <- "yelpResData.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,dbname = filename)
dbListTables(db)
review <- dbReadTable(db,"review")

#made a sample data
review_subset <- review[,c('reviewContent','rating')]
##print(nrow(review_subset))->total 788471rows
train_data<-review_subset[1:630000,]
#validation_data<-review_subset[630001,709236]
test_data<-tail(review_subset,158471)
##make random sample data
#set.seed(123) 
#num_samples <- 10
#sample_size <- 800
#sub_samples <- list()
#for (i in 1:num_samples) {
#indices <- sample(nrow(train_data), sample_size, replace = FALSE)
#assign(paste0("sub_sample_", i), train_data[indices, ], envir = .GlobalEnv)
#}
sample_data_1 <-train_data[1:800,]
sample_data_2 <-train_data[1:1000,]
sample_data_3 <-train_data[1:1200,]
sample_test_data<-test_data[1:800,]




#extract all words from review DB
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

lr_mse <- list()
xgboost_mse <- list()
lasso_mse <- list()

#linear_regression
#get rid of Multicollinearity
library(corrplot)
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

#lasso
library(glmnet)
library(readr)

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

#xgboost
library(xgboost)
library(readr)

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


# Combine MSE values into a data frame
mse_data <- data.frame(
  Method = rep(c("Linear Regression", "XGBoost", "Lasso"), each = 3),
  SubsampleSize = rep(c(800, 1000, 1200), times = 3),
  MSE = c(lr_mse[[1]], lr_mse[[2]], lr_mse[[3]], xgboost_mse[[1]], xgboost_mse[[2]], xgboost_mse[[3]], lasso_mse[[1]], lasso_mse[[2]], lasso_mse[[3]])
)

# Print the resulting data frame
print(mse_data) 










library(RSQLite)
library(NLP)
library(stringr)
library(tm)
library(corrplot)
library(glmnet)
library(xgboost)
library(readr)
library(dplyr)


predictor <- function(train_data, test_data, method = "Lasso") {
  # Load and prepare the database (if not passed as dataframe directly)
  if (is.character(train_data) && is.character(test_data)) {
    db <- dbConnect(RSQLite::SQLite(), dbname = "yelpResData.db")
    train_data <- dbReadTable(db, train_data)
    test_data <- dbReadTable(db, test_data)
    dbDisconnect(db)
  }
  
  mse_results <- list()
  # Process text data to extract features
  all_words <- unlist(str_extract_all(tolower(unlist(train_data$reviewContent)), "\\b\\w+\\b"))
  word_freq <- table(all_words)
  word_freq_df <- as.data.frame(word_freq, stringsAsFactors = FALSE)
  names(word_freq_df) <- c("word", "Freq")
  top_words <- head(word_freq_df[order(-word_freq_df$Freq),], 500)
  stop_words <- stopwords("en")
  selectedwords <- setdiff(top_words$word, stop_words)
  
  # Add word count features
  add_word_count_features <- function(data, words) {
    for (word in words) {
      counts <- sapply(data$reviewContent, function(row) sum(str_count(tolower(row), word)))
      data[[paste0("count_", word)]] <- counts
    }
    return(data)
  }
  
  train_data <- add_word_count_features(train_data, selectedwords)
  test_data <- add_word_count_features(test_data, selectedwords)
  
  # Model fitting and MSE calculation
  if (tolower(method) == "linear") {
    formula <- as.formula(paste("rating ~", paste(grep("^count_", names(train_data), value = TRUE), collapse = " + ")))
    model <- lm(formula, data = train_data)
    predictions <- predict(model, newdata = test_data)
    mse <- mean((test_data$rating - predictions)^2)
  } else if (tolower(method) == "lasso") {
    train_X <- as.matrix(train_data[, grep("^count_", names(train_data), value = TRUE)])
    train_Y <- train_data$rating
    test_X <- as.matrix(test_data[, grep("^count_", names(test_data), value = TRUE)])
    cv_lasso_model <- cv.glmnet(train_X, train_Y, alpha = 1)
    predictions <- predict(cv_lasso_model, s = "lambda.min", newx = test_X)
    mse <- mean((test_data$rating - predictions)^2)
  } else if (tolower(method) == "xgboost") {
    train_X <- as.matrix(train_data[, grep("^count_", names(train_data), value = TRUE)])
    train_Y <- train_data$rating
    dtrain <- xgb.DMatrix(data = train_X, label = train_Y)
    dtest <- xgb.DMatrix(data = test_X, label = test_data$rating)
    params <- list(objective = "reg:squarederror", eta = 0.1, max_depth = 6, eval_metric = "rmse")
    xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100)
    predictions <- predict(xgb_model, dtest)
    mse <- mean((test_data$rating - predictions)^2)
  }

  mse_results[[method]] <- mse
  print(mse_results)
}
predictor(train_data=sample_data_3, test_data=sample_test_data, method = "Lasso")

dataLoader <- function(data,num){
  # Randomly sample 'num' rows from the data frame
  sampled_data <- data[sample(nrow(data), num, replace = FALSE), , drop = FALSE]
  
  # Assuming there is a column 'reviewContent' in the data
  # Process text data to extract word frequencies
  word_list <- strsplit(tolower(sampled_data$reviewContent), "\\W+")  # Split by non-word characters
  words <- unlist(word_list)  # Flatten the list of words
  word_freq <- table(words)   # Create a table of word frequencies
  
  # Return the word frequencies as a table
  return(word_freq)
}
dataLoader <- function(sentences) {
  # Check if input is a list of character strings
  if (!is.list(sentences) || !all(sapply(sentences, is.character))) {
    stop("The input must be a list of character strings.")
  }
  
  # Concatenate all sentences into a single string (if needed)
  combined_text <- tolower(unlist(sentences))
  
  # Split the combined text into words
  words <- unlist(strsplit(combined_text, "\\W+"))
  
  # Create a table of word frequencies
  word_freq <- table(words)
  
  # Remove empty entries if any (due to splitting non-word characters)
  word_freq <- word_freq[names(word_freq) != ""]
  
  return(word_freq)
}
# Create a list of sentences
sentences <- list('I like it', 'this place is so bad', 'I will never come back again')
dataLoader(sample_data_1,3)
dataLoader(sentences)


