# Logistic Regression

# We'll use data_processing_template to start

# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[, 3:5]
# Since we only want to use age and salary to predit purchase, so we only want columns 3, 4, and 5



# Splitting the dataset into the Training set and Test set
# We want 300 taining and 100 test out of our total 400 observations
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# We will only scale columns (indexes) 1 and 2
training_set[, 1:2] = scale(training_set[, 1:2])
test_set[, 1:2] = scale(test_set[, 1:2])

# Fitting Logistic Regression to the Training set
# We use GLM since logistic regression is a linear classifier
# For logistic regression we need to specify a binomial family
classifier = glm(formula = Purchased ~ ., 
                 family = binomial,
                 data = training_set)


# Predicting the Test set results
# We'll predict the probabilities first
# For LR we use the 'response' type to list the probabilities in a single vector
# The new data will be only the first 2 columns of the test set, we can remove the 3rd with [-3] OR [, 1:2]
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])

# type prob_pred in console to get our probabilities, we see for line 2 (the 1st observation) 
# it predicts 0.016 or 1.6% and we can see from the original test set this agrees with the 0 in column 3
# signifying they did not purchase the car

#we can turn these probabilities into 0 or 1 predictions 
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#now type y_pred into console for the direct predictions


# Making the Confusion Matrix
# This will count the number of correct and incorrect predictions from our y_pred
#we specify real values from test_set column 3, and predicted values from y_pred
cm = table(test_set[, 3], y_pred)

#type cm into console, we see y_pred on top and real values on left
#y_pred guessed 0 when true value was 0 57 times, it guessed 0 incorrectly 10 times
#y_pred guessed 1 correctly 26 times and guessed 1 incorrectly 7 times
# total 83 correct and 17 incorrect predictions

# Note: the code below was pre-written
# Visualising the Training set results
library(ggplot2)
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# the points are the observations of the training set
# red points mean Purchased = 0, green means Purchased = 1 in training set
# We can see that older users bought the SUV with higher and lower salaries
# The red and green areas are the prediction regions
# our classifier predicts ALL users in red area will not buy SUV while ALL users in green area will
# The straight line is the prediction boundary. If the prediction boundary is straight, our classifier is linear
# We will build non-linear classifiers later, their prediction boundaries will not be straight

# If the point and region color don't match, that is an inaccurate prediction
# our regions will not change going from the training to test set, since it learned off the training set


# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))


# Most points are in the right region, pretty good. It's normal for linear classifiers to have some inaccurate predictions