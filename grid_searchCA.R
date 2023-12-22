# Grid Search
## There are 2 types of ML parameters, those we choose and those the model learns for itself
## Parameters we choose/specify are known as hyperparameters (ex. the kernel for SVM)
## Grid search finds the optimal values for the hyperparameters, which makes a better model
## Here we will use the caret package to both build and grid-search optimize our SVM
## Note: caret is probably the best package for ML, you can build any model and optimize it at the same time


# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting Kernel SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = Purchased ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

# Applying k-Fold Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(training_set$Purchased, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = svm(formula = Purchased ~ .,
                   data = training_fold,
                   type = 'C-classification',
                   kernel = 'radial')
  y_pred = predict(classifier, newdata = test_fold[-3])
  cm = table(test_fold[, 3], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))

# Applying Grid Search to find the best parameters
# install.packages('caret')
library(caret)
classifier = train(form = Purchased ~ ., data = training_set, method = 'svmRadial')
classifier
classifier$bestTune



## If you go here: https://topepo.github.io/caret/available-models.html
## and click 6 in the sidebar you can see available models within the caret package
## we will be using 'Support Vector Machines with Radial Basis Function Kernel'
## the 2nd column on this page shows the method we input for the train() function, which is svmRadial
## when we execute line 64: classifier, we get a report in the console that says
## "Accuracy was used to select the optimal model using the largest value."
## This shows that model accuracy was the performance metric used to tune the model
## The "final values" shown after that are the best values for hyperparameters for the optimal accuracy (sigma = 1.43 and C = 1
## Looking at the section above we even see the accuracy of our model with C=1 would be 91%
## Above that we see Resampling: Bootstrapped (25 reps), this is the method used to train the model and this is the exact same method as k-fold cross validation
## Executing Classifier$bestTune spits out just the optimal parameters without the rest of the report
## To use these parameters, go to the "# Fitting Kernel SVM to the Training set" and use them in the classifier we made
## Which would look like this:
## classifier = svm(formula = Purchased ~ ., data = training_set, type = 'C-classification', kernel = 'radial', gamma = 1.433809, cost = 1)
## note that 'sigma' in the caret package is called 'gamma' in the e1071 package we used for the svm classifier towards the top, and C is called cost
## in caret, sigma is a value, so that means it is an output of the model and not something you specify while building it
## note: I tried both with and without our new parameters, without had a better cm ;_;



# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Kernel SVM (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'Kernel SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))