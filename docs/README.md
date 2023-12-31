# Applying Grid Search to Kernel SVM
This is a simple demonstration of applying the Grid Search method to find the optimal parameters for a Kernel SVM model. 

### Grid Search
There are 2 types of ML parameters, those we choose and those the model learns for itself. Parameters we choose/specify are known as hyperparameters (ex. the kernel for SVM). Grid search finds the optimal values for the hyperparameters, which makes a better model. Here we will use the caret package to both build and grid-search optimize our SVM. Note: caret is probably the best package for ML, you can build any model and optimize it at the same time.

### Our Data
We will be using a data set that describes whether or not a certain user purchased a product after being shown an ad for it. It includes the following categories: User ID, Gender, Age, EstimatedSalary, Purchased where Purchased = 0 means the item was not purchased and Purchased=1 means it was. 

### The Process
Importing the dataset:
```{r setup, include=TRUE, eval=TRUE}
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]
```
Encoding the target feature (Purchased) as factor:
```{r setup, include=TRUE, eval=TRUE}
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))
```