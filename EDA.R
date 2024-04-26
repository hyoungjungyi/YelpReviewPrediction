library(RSQLite)
setwd("/Users/hyeongjeongyi/Documents/RA")

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
test_data<-tail(review_subset,n=158471)
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
sample_test_data<-test_data[1:800,1,drop=FALSE]




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
  sample_test_data[paste0("count_", word)] <- sapply(sample_test_data$reviewContent, count_word_occurrences, word)
}

for (i in 1:3) {
  data_name <- paste0("sample_data_", i)
  for (word in selectedwords) {
    sample_data <- get(data_name)
    sample_data[paste0("count_", word)] <- sapply(sample_data$reviewContent, count_word_occurrences, word)
    assign(data_name, sample_data)
  }
}



