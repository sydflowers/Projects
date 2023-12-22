# PCA

# Importing the dataset
dataset = read.csv('Wine.csv')

#This data includes wine components and the Customer Segment (group of customers) that prefer that wine combo
#So for each new wine we can predict which customers will like the wine
#We also want to see the prediction boundaries, but we have too many ind. variables
#So we will extract the 2 most important wine components with PCA
#The resulting 2 ind. variables will be NEW, they will NOT be selected from original columns


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


# Applying PCA
install.packages('caret')
library(caret)
install.packages('e1071')
library(e1071)

pca = preProcess(x = training_set[-14], method = 'pca', pcaComp =  2)
  #pca will be used to transform our dataset from the original 13 ind var. to the new 2
  #pcaComp is the # of ind. variables you want to end up with (we want 2)

training_set = predict(pca, training_set)
  #this predict function will apply the pca transformation to our training set
  #now our training set has just 2 columns, Principal Component 1 and 2
  #Dep. var is moved to first position (column)

training_set = training_set[c(2, 3, 1)]
  #this re-orders the training set columns to have the 2 ind var first, then dep var
  #this step is needed so it's in the corrct order for later

test_set = predict(pca, test_set)
test_set = test_set[c(2, 3, 1)]
  #we want to transform the test set too


# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = Customer_Segment ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
  #cm shows 100% accuracy

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'PC1', ylab = 'PC2',
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
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))
  #test set results are similar, with 100% accuracy in wine placement

