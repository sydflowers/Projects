# Natural Language Processing (Bag of Words model)

# Importing the dataset
#Note the dataset is a .tsv NOT .csv
#The delimiter is a tab, not a comma
#This is used because some reviews have a comma in them
#This would create problems when doing text to columns
#In the Liked column, 1 means good and 0 means bad

dataset_original = read.delim('Restaurant_Reviews.tsv', quote = '', stringsAsFactors = FALSE)
#read.delim will read a .tsv file
#quote = '' means it will ignore the quotes in the reviews
#stringsAsFactors = FALSE means it will not interpret strings as factors

#Imports nicely into 2 columns

# Cleaning the Texts (Compulsory step for NLP)
install.packages('tm')
library(tm)
install.packages('SnowballC')
library(SnowballC)
#to prep the data we will break the reviews out into a sparse matrix
#1000 rows (1 for each review)
#a BUNCH of columns, 1 column for each word used in a review
#a cell will have a 1 if that review contained that word, 0 if it didnt
#if it contains the word 3 times it will have a 3
#We will call this table corpus
#"

corpus = VCorpus(VectorSource(dataset$Review))

#we will make all words lowercase so we dont have duplicate columns with different case for the same word

corpus = tm_map(corpus, content_transformer(tolower))

#type as.character(corpus[[1]]) into console to return review 1
#check before and after running line 32 to see the lowercase change

#next we will remove all irrelevant numbers in the reviews

corpus = tm_map(corpus, removeNumbers)

#type as.character(corpus[[841]]) in console to see a review with numbers
#execute 39 and type ^ again to see it removed

#Next we remove any punctuation, we don't want columns for those, just relevant words

corpus = tm_map(corpus, removePunctuation)

#type as.character(corpus[[1]]) before and after executing 46 to see change
#"wow... loved this place." becomes "wow loved this place"

#Next we remove non-relevant words (this, the, etc.)
#stopwords is a built-in list of non-relevant words, commonly used in NLP ML
#we need SnowballC package to use stopwords

corpus = tm_map(corpus, removeWords, stopwords())

#using as.character(corpus[[1]]), we see review 1 becomes: 
#"wow loved  place"

#Next we'll get the root of the words (i.e. "loved" becomes "love")
#We'll just keep the roots, reducing total number of columns
#This is called stemming

corpus = tm_map(corpus, stemDocument)

# "wow loved  place" becomes "wow love place"

#Next we will remove extra spaces, these resulted from removing words

corpus = tm_map(corpus, stripWhitespace)

#as.character(corpus[[841]]) shows "really" became "realli"
#That's how R roots that word, not a mistake


# Creating the Bag of Words model
#DocumentTemMatrix will create our sparse matrix

dtm = DocumentTermMatrix(corpus)

#ncol is 1577, so we found 1577 unique, relevant root words, 1 column for each
#we will filter for the most frequent words, low frequency is less relevant word
#type dtm into console, Sparsity = 100%

dtm = removeSparseTerms(dtm, 0.999)

#this will keep 99.9% of the most frequent words
#we only have 1000 reviews so we don't want to remove too many words
#now dtm shows 691 columns
#type dtm in console, sparsity is now 99%, less 0's than before

#we'll use a previous classification model to build our bag of words
#most common for NLP: naive bayes, descision tree or random forest
#we'll use random forest
#BUT we need our dataset in a data frame for RF, not a matrix which is what dtm is now
#as.data.frame does this easily
#we also have to use as.matrix so our dtm is the expected type of input for as.data.frame

dtm = as.matrix(dtm)
dataset1 = as.data.frame(dtm)
dataset1$Liked = dataset_original$Liked


#dataset1$Liked = dataset_original$Liked adds a new "Liked" column to our new data frame
#by using the Liked column from the original data, our dependent variable
# Pasted below is the code from the random forest project
#note we can NOT use the visualization code since it is only for 2 dimensions
#we have ~600 columns (independent variables) = ~600 dimensions

# Encoding the target feature as factor
dataset1$Liked = factor(dataset1$Liked, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset1$Liked, SplitRatio = 0.8)
training_set = subset(dataset1, split == TRUE)
test_set = subset(dataset1, split == FALSE)

# Feature Scaling not needed

# Fitting Random Forest Classification to the Training set
install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-692], 
                          y = training_set$Liked, 
                          ntree = 10)

#training_set[-692] removed the Liked column from training set
#note that x is a matrix (data frame) of in. variables and y is a vector from the F1 notes of randomForest()
#ntree is number of trees, we'll pick 10 for now

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692])

# Making the Confusion Matrix
cm = table(test_set[, 692], y_pred)

#type cm in console, we see more correct than incorrect predictions
#accuracy is #correct / #total observations
# for this, (82+77)/200 = 79.5%


#Below is a cleaner version of the same code



dataset_original = read.delim('Restaurant_Reviews.tsv', quote = '', stringsAsFactors = FALSE)

# Cleaning the texts
# install.packages('tm')
# install.packages('SnowballC')
library(tm)
library(SnowballC)
corpus = VCorpus(VectorSource(dataset_original$Review))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)

# Creating the Bag of Words model
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)
dataset = as.data.frame(as.matrix(dtm))
dataset$Liked = dataset_original$Liked

# Encoding the target feature as factor
dataset$Liked = factor(dataset$Liked, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692])

# Making the Confusion Matrix
cm = table(test_set[, 692], y_pred)