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

dataLoader<-function(sentences = NULL, data = NULL, num = NULL){
  if(!is.null(sentences)){
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
  if(!is.null(data)&&!is.null(num)){
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
  else{
    print("invalid input")
    stop()
  }
}


# Create a list of sentences
sentences <- list('I like it', 'this place is so bad', 'I will never come back again')
dataLoader(data=sample_data_1,num=3)
dataLoader(sentences)

predictor(train_data=sample_data_1, test_data=sample_test_data, method = "Lasso")


extract_features <- function(review, selected_words) {
  # Count occurrences of each selected word in the review
  feature_counts <- sapply(selected_words, function(word) sum(str_count(tolower(review), word)))
  feature_names <- paste0("count_", selected_words)
  features <- as.data.frame(t(feature_counts))
  names(features) <- feature_names
  return(features)
}

my_predicted_model <- function(new_review) {
  # Sample data creation for demonstration
  reviews <- c("Great place with wonderful desserts!", 
               "Horrible service, will not return.", 
               "Average experience, nothing special.")
  ratings <- c(5, 1, 3)
  
  # Create a Corpus from reviews
  corpus <- Corpus(VectorSource(reviews))
  
  # Text processing
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Create a document-term matrix
  dtm <- DocumentTermMatrix(corpus)
  
  # Convert dtm to matrix for glmnet
  dtm_matrix <- as.matrix(dtm)
  dtm_matrix[is.na(dtm_matrix)] <- 0
  
  # Fit a linear model using glmnet (good for sparse data)
  fit <- cv.glmnet(x = dtm_matrix, y = as.numeric(ratings), alpha = 1)
  
  # Feature extraction from new review
  new_corpus <- Corpus(VectorSource(c(new_review)))
  new_corpus <- tm_map(new_corpus, content_transformer(tolower))
  new_corpus <- tm_map(new_corpus, removePunctuation)
  new_corpus <- tm_map(new_corpus, removeNumbers)
  new_corpus <- tm_map(new_corpus, removeWords, stopwords("en"))
  new_corpus <- tm_map(new_corpus, stripWhitespace)
  
  # Create a document-term matrix for the new review
  new_dtm <- DocumentTermMatrix(new_corpus, control = list(dictionary = Terms(dtm)))
  new_dtm_matrix <- as.matrix(new_dtm)
  new_dtm_matrix[is.na(new_dtm_matrix)] <- 0
  
  # Prediction
  predicted_rating <- predict(fit, s = "lambda.min", newx = new_dtm_matrix)
  
  # Round to the nearest integer and ensure the rating is between 1 and 5
  predicted_rating <- round(predicted_rating)
  predicted_rating <- max(1, min(5, predicted_rating))
  
  # Output the predicted rating
  return(predicted_rating)
}
predicted_rating <- my_predicted_model("The food was delicious and the staff were very friendly.")
print(predicted_rating)


