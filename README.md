# Yelp Rating Prediction Model
## Description
This vignette illustrates an exploratory data analysis (EDA + modeling) performed on Yelp restaurant reviews. The primary goal is to understand the sentiments conveyed through reviews and predict the rating by analyzing the frequency and context of words used.

## Usage
```R
dataLoader(data,numberOfReviews)
dataLoader(sentences)
predictor(train_data, test_data, method = "Lasso")
```

## Functions
**dataLoader(data,numberOfReviews)** : randomly select three reviews and output the corresponding word frequencies.  
**dataLoader(sentences)** : output the corresponding word frequencies in the list of sentences.  
**predictor(train_data, test_data, method)** : output the mse of model training output    
**my_prediction_model(my_review)** : put your line of review inside and get the predicted rating

## Arguments
**numberOfReviews** : size of the subclass you want to create  
**sentences** : is a list of reviews  
**train_data** : put the name of your train data. This data frame will be used to train the model      
**test_data** : put the name of your test data. This data frame will be used to evaluate the model, by calculating the MSE value.     
**method** : Choose between "Linear Regression", "Lasso", and "XGBoost" for the method used in the model    

## Details
This is an exploratory data analysis process using an SQLite database containing Yelp restaurant reviews. 
The EDA.R file focuses on processing text for natural language tasks, and preparing data for modeling, all within the context of analyzing Yelp reviews to possibly predict ratings and understand sentiment. The focus is on practical data handling, extraction, and preprocessing steps that are foundational in data science workflows.

## Examples
```R
> predictor(train_data=sample_data_3, test_data=sample_test_data, method = "Lasso")
$Lasso
[1] 1.811803
```
```R
> dataLoader(data=sample_data_1,num=3)
words
               10                11               200 
                1                 1                 1 
               30                 4                40 
                1                 2                 1 
                5                 6                60 
```
```R
> sentences <- list('I like it', 'this place is so bad', 'I will never come back again')
> dataLoader(sentences)
words
again  back   bad  come     i    is    it  like never place    so  this  will 
    1     1     1     1     2     1     1     1     1     1     1     1     1 
```
```R
> predicted_rating <- my_predicted_model("The food was delicious and the staff were very friendly.")
> print(predicted_rating)
3
```

