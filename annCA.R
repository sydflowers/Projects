# Artificial Neural Network
#we'll use the Classification Template from Section 3 for preprocessing

# Importing the dataset
dataset = read.csv('Churn_Modelling.csv')
dataset = dataset[4:14]

#we don't want Row Number, Customer ID, or Surname in the dataset since these do
#NOT impact the result (exited or not)
#So we'll take out indexes 1-3 out
#we WILL include the dependent variable for now

# Encoding the target feature as factor
#we do NOT need to encode our d.v. (Exited) since it is already binary
#we DO need to encode categorical variables (Geography, Gender) as factors
#our package wants them as NUMERICAL factors as well
#we will use the categorica_data template from section 1: Data Pre-Processing

dataset$Geography = as.numeric(factor(dataset$Geography, 
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender, 
                                   levels = c('Female', 'Male'), 
                                   labels = c(1, 2)))


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling - 100% compulsory for training ANN's
training_set[-11] = scale(training_set[-11])
test_set[-11] = scale(test_set[-11])

# Fitting classifier to the Training set
#We'll use h2o package, works very efficiently because it connect to computer network
#good to use for compute-intensive stuff
#it contains a parameter tuning argument too (good)
install.packages('h2o')
library(h2o)

#first step is to establish the connection to an h2o instance
#nthreads = -1 will connect to the default server with max number of cores (good)
#random TIP: press tab key in open parenthesis to see drop-down of possible arguments
h2o.init(nthreads = -1)
classifier = h2o.deeplearning(y = 'Exited',
                              training_frame = as.h2o(training_set),
                              activation = 'Rectifier',
                              hidden = c(6,6),
                              epochs = 100,
                              train_samples_per_iteration = -2)

#training set needs to be an h2o data frame, so we use as.h2o
#our activation function will be the rectifier function, see intuition video for more info
#most convenient # of hidden nodes in the hidden layers is the average of input and output nodes
#we have 10 nodes for the hidden layer, since we have 10 independent variables
#we have 1 output layer node since the outcome is binary
#(10+1)/2 = 5.5 ~ 6, we'll pick 6 nodes for the hidden layer
#we'll use 2 hidden layers, for fun
#input for "hidden" argument is a vector, which will have 2 spots for 2 hidden layers
#the number in each spot is the # of nodes that we choose
#c(6,6) has 2 hidden layers, each with 6 nodes
#train_samples_per_iteration specifies how many iterations will be done before the weights are adjusted = batch size
#defaults to -2, which will go through all data before updating weights
#since we're connected to a server, the model trains much faster than python


# Predicting the Test set results
prob_pred = h2o.predict(classifier, newdata = as.h2o(test_set[-11]))
y_pred = (prob_pred > 0.5)
y_pred = as.vector(y_pred)

#prob_pred comes out as an Environment object, so the y_pred line just assigns a 
#value of 0 or 1 based on the probability (>0.5 = 1, <0.5 = 0)
#this will make a boolean called y_pred of our predictions as true/false
#you can also write y_pred = ifelse(prob_pred > 0.5, 1, 0) 
#y_pred needs to be a vector to go into the confusion matrix function

# Making the Confusion Matrix
cm = table(test_set[, 11], y_pred)

#we see 1535 + 196 correct, and 211 + 58 incorrect
# (1535 + 196)/2000 = 86.5% accuracy, not bad

h2o.shutdown()
#this line disconnects us from the h2o server, always good to do
#Type Y in console after executing this line and it will disconnect
