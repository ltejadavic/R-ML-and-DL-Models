#Natural Language Processing

#Importing the dataset
setwd("~/Downloads/Machine Learning Course/Section 34")

#Cause the reviews has comas is not possible to use csv
#that's why we'll use tsv

dataset = read.delim('Restaurant_Reviews.tsv', quote = '', stringsAsFactors = F)

#Cleaning the Texts
install.packages('tm')
install.packages("SnowballC")
library(tm)
library(SnowballC)

#corpus is a matrix of list, with to lower transform from upper case to lower
corpus = VCorpus(VectorSource(dataset$Review))
corpus = tm_map(corpus, content_transformer(tolower))
#here we are removing numbers
corpus = tm_map(corpus, removeNumbers)
#removing punctuation
corpus = tm_map(corpus, removePunctuation)
#removing not relevant words, stopwords function gives a library with non useful
#words as the, of, in.
corpus = tm_map(corpus, removeWords, stopwords())
#Stemming step, getting the root of the word, loved, loves, loving, ... = love
corpus = tm_map(corpus, stemDocument)
#dissapear extra whitespaces created with previous steps 
corpus = tm_map(corpus, stripWhitespace)

#Creating the Bag of Words model
dtm = DocumentTermMatrix(x = corpus)
#after this object was created, the #of columns is equal to the number of words
#aplying filters to remove the nos frequent words cause they didn't help with
#correlations
dtm = removeSparseTerms(x = dtm, sparse = 0.999)

#Building a Machine Learning Classification Model
#transforming matrix into a dataframe
dataset = as.data.frame(as.matrix(dtm))
dataset_original = read.delim('Restaurant_Reviews.tsv', quote = '', stringsAsFactors = F)
dataset$Liked = dataset_original$Liked

#Encoding the target feature as factor
dataset$Liked = factor(x = dataset$Liked, levels = c(0,1))

#Spliting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split=sample.split(Y = dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split== F)


#Fitting Naive Random Forest Classification to the Training set and Predicting Results
library(randomForest)
classifier = randomForest(x = training_set[-692], y = training_set$Liked,
                          ntree = 10)

#Predicting the Test set result
y_pred = predict(object = classifier, newdata = test_set[-692])

#Making the Confusion Matrix
library(caret)
confusionMatrix(factor(y_pred), factor(test_set$Liked), positive = '1')
cm = confusionMatrix(factor(y_pred), factor(test_set$Liked), positive = '1')