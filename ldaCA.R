# LDA
  # LDA is supervised unlike PCA, it extracts new independent variables that most impact the classification of the dep. var

# Importing the dataset
dataset = read.csv('Wine.csv')


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Feature Scaling (Needed for PCA)
#We'll scale all the variables except the Dep. variable
training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])


# Applying LDA 
library(MASS)
lda = lda(formula = Customer_Segment ~ ., data = training_set)

  #there will be k-1 linear discriminants where k is the # of classes in dep. variable
  #our data has 3 Customer_Segment classes, so we will get 2 linear discriminants (exactly what we want for dimensionality reduction)
  #our new "extracted features" or "new" variables are the linear discriminants
training_set = as.data.frame(predict(lda, training_set))
  #now training_set has the customer segment as 1st column, re-named 'class' by predict function
  #LD1 and LD2 are the linear discriminants/new extraced features
  #the 3 posteriors are variables derived by the lda equation (not important)

  #we need to re-order the columns and exclude the posteriors, as D1 LD2 Class
training_set = training_set[c(5, 6, 1)]

test_set = as.data.frame(predict(lda, test_set))
test_set = test_set[c(5, 6, 1)]
  #here we did the same for the test set, we need this format for later sections


# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = class ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')
  #note that 'class' here is our dependent variable in training set

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
#cm shows 1 incorrect prediction, that's ok


# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')
  #colnames must have real names of your ind. variables

y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'LD1', ylab = 'LD2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))
#we can see all the dots are in their correct regions, so the SVM correctly predicted which wines go in each customer group



# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'LD1', ylab = 'LD2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))
  #shows 1 incorrect prediction (dot outside prediction boundary)

#this was a linear problem, so we used linear feature extraction (LDA and PCA)

