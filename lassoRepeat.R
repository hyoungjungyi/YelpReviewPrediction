
library(RSQLite)
library(stringr)
library(tm)
library(glmnet)

# Assuming 'train_data' and 'sample_test_data' are already loaded and available
sample_list <- list(sample_data_1, sample_data_2, sample_data_3, sample_data_4, sample_data_5,
                    sample_data_6, sample_data_7, sample_data_8, sample_data_9, sample_data_10)

lasso_mse <- list()

for (j in 1:length(sample_list)) {
  sample_data <- sample_list[[j]]
  
  # Extract all words from review DB
  text_data <- sample_data$reviewContent
  all_words <- unlist(lapply(text_data, function(sentence) str_extract_all(sentence, "\\b\\w+\\b")))
  
  # Print word frequency
  word_freq <- table(all_words)
  word_freq_df <- as.data.frame(word_freq, stringsAsFactors = FALSE)
  names(word_freq_df) <- c("Word", "Freq")
  top_words <- head(word_freq_df[order(-word_freq_df$Freq), ], 500)
  
  # Get rid of unnecessary words
  stop_words <- stopwords("english")
  selectedwords <- setdiff(top_words$Word, stop_words)
  
  # Count the relevant words in each row
  for (word in selectedwords) {
    counts <- sapply(sample_data$reviewContent, function(row) sum(str_count(row, fixed(word))))
    sample_data[[paste0("count_", word)]] <- counts
  }
  
  # Prepare data for Lasso
  train_X <- as.matrix(sample_data[, grep("^count_", names(sample_data))])
  train_Y <- sample_data$rating
  
  if (ncol(train_X) > 0) {
    cv_lasso_model <- cv.glmnet(train_X, train_Y, alpha = 1)
    # Make predictions
    predictions <- predict(cv_lasso_model, s = "lambda.min", newx = train_X)
    # Calculate MSE
    mse <- mean((train_Y - predictions)^2)
    lasso_mse[[j]] <- mse
  } else {
    lasso_mse[[j]] <- NA  # No valid model could be fitted
  }
}

# Print the MSE for each sample
print(lasso_mse)







library(glmnet)
library(stringr)
library(tm)

# Assuming sample_list is already defined and contains your 10 sample data frames

positive_coefficients_list <- list()
negative_coefficients_list <- list()

for (i in 1:length(sample_list)) {
  sample_data <- sample_list[[i]]
  
  # Text processing and feature extraction
  words <- unlist(lapply(sample_data$reviewContent, function(sentence) str_extract_all(sentence, "\\b\\w+\\b")))
  word_freq <- table(words)
  word_freq_df <- as.data.frame(word_freq, stringsAsFactors = FALSE)
  names(word_freq_df) <- c("Word", "Freq")
  
  stop_words <- stopwords("english")
  filtered_words <- word_freq_df[!word_freq_df$Word %in% stop_words,]
  top_words <- head(filtered_words[order(-filtered_words$Freq),], 500)$Word
  
  # Create feature matrix
  feature_matrix <- sapply(top_words, function(word) {
    sapply(sample_data$reviewContent, function(text) sum(str_count(text, fixed(word))))
  })
  
  if (ncol(feature_matrix) < 2) {
    positive_coefficients_list[[i]] <- NA  # Record NA if not enough features
    negative_coefficients_list[[i]] <- NA
    next  # Skip this sample if there are not enough features
  }
  
  # Convert to matrix and add response variable
  train_X <- as.matrix(feature_matrix)
  train_Y <- sample_data$rating
  
  # Lasso regression
  cv_lasso_model <- cv.glmnet(train_X, train_Y, alpha = 1)
  coefficients <- coef(cv_lasso_model, s = "lambda.min", exact = TRUE)
  non_zero_coefficients <- coefficients[coefficients[, 1] != 0, , drop = FALSE]
  positive_coefficients <- rownames(non_zero_coefficients)[non_zero_coefficients[, 1] > 0]
  negative_coefficients <- rownames(non_zero_coefficients)[non_zero_coefficients[, 1] < 0]
  
  # Exclude the intercept from the lists
  positive_coefficients <- positive_coefficients[-1]
  negative_coefficients <- negative_coefficients[-1]
  
  # Store the coefficients in their respective lists
  positive_coefficients_list[[i]] <- positive_coefficients
  negative_coefficients_list[[i]] <- negative_coefficients
}

# Print the lists of coefficients for all models
for (i in 1:length(positive_coefficients_list)) {
  cat(sprintf("Model %d Positive Coefficients:\n", i))
  print(positive_coefficients_list[[i]])
  cat(sprintf("Model %d Negative Coefficients:\n", i))
  print(negative_coefficients_list[[i]])
}

all_positive_counts <- table(unlist(positive_coefficients_list))
all_negative_counts <- table(unlist(negative_coefficients_list))

# Sorting the counts in decreasing order
sorted_positive_counts <- sort(all_positive_counts, decreasing = TRUE)
sorted_negative_counts <- sort(all_negative_counts, decreasing = TRUE)

positive_counts_df <- data.frame(Word = names(sorted_positive_counts), Count = as.integer(sorted_positive_counts), row.names = NULL)
negative_counts_df <- data.frame(Word = names(sorted_negative_counts), Count = as.integer(sorted_negative_counts), row.names = NULL)
write.csv(positive_counts_df, "sorted_positive_counts.csv", row.names = FALSE)
write.csv(negative_counts_df, "sorted_negative_counts.csv", row.names = FALSE)














# Make predictions
predictions <- predict(cv_lasso_model, s = "lambda.min", newx = train_X)
differences <- abs(predictions - train_Y)
# Determine the threshold
threshold <- quantile(differences, 0.95)
large_diff_indices <- which(differences > threshold)
large_difference_reviews <- train_data[large_diff_indices,]
large_difference_reviews_df <- data.frame(ReviewContent = large_difference_reviews)
write.csv(large_difference_reviews_df, "large_difference_reviews.csv", row.names = FALSE)
write.csv(large_difference_reviews_df, "large_difference_reviews.csv", row.names = FALSE)


help.lm(vignette)
help("lm")




better_index <- which(colnames(feature_matrix) == "better")
if (length(better_index) > 0 && coef(cv_lasso_model, s = "lambda.min")[better_index + 1, 1] < 0) {
  rows_with_better <- train_data[grepl("\\bbetter\\b", train_data$reviewContent, ignore.case = TRUE), ]
  significant_better_rows <- rows_with_better[feature_matrix[grepl("\\bbetter\\b", rows_with_better$reviewContent, ignore.case = TRUE), "better"] > 0, ]
  write.csv(significant_better_rows, "significant_better_rows.csv", row.names = FALSE)
  cat("File saved: significant_better_rows.csv\n")
}

for (sentence in sample_data){
  counts <- sapply(sample_data$reviewContent, function(row) sum(str_count(row, fixed(word))))
  print(counts)
}

