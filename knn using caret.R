library(caret)

# Create index to split based on labels
index <- createDataPartition(iris$Species, p=0.75, list=FALSE)

# Subset training set with index
iris.training <- iris[index,]

# Subset test set with index
iris.test <- iris[-index,]

# Overview of algos supported by caret
names(getModelInfo())

# Train a model
model_knn <- train(iris.training[, 1:4], iris.training[, 5], method='knn')

model_cart <- train(iris.training[, 1:4], iris.training[,5], method='rpart2')

# Predict the labels of the test set
predictions <- predict(object=model_knn, iris.test[,1:4])

# Evaluate the predictions
table(predictions)

# Confusion matrix
confusionMatrix(predictions, iris.test[,5])
