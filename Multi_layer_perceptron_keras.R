devtools::install_github("rstudio/keras")
library(keras)
devtools::install_github("rstudio/tensorflow")
library(tensorflow)
install_tensorflow()

# Read in iris data
iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE)

# Return the first part of iris
head(iris)

# Inspect the structure
str(iris)

# obtain the dimensions
dim(iris)

names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
plot(iris$Petal.Length, iris$Petal.Width, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)],xlab="Petal Length", ylab="Petal Width")

# Overall correlation between Petal.Length and Petal.Width
cor(iris$Petal.Length, iris$Petal.Width)

install.packages("corrplot")
library(corrplot)
#  Store the overall correlation in M
M <- cor(iris[,1:4])

# Plot the correlation plot with M
corrplot(M, method="circle")

# Pull up a summary of iris
summary(iris)

# Inspect the structure of iris
str(iris)
hist(as.matrix(iris[,1:4]))
normalize <- function(x){
  num <- x -min(x)
  denum <- max(x) - min(x)
  return (num/denum)
}

iris_norm <- as.data.frame(lapply(iris[1:4],normalize))
head(iris_norm)
hist(iris_norm[1:4])

#iris[,5] <- as.numeric(iris[,5])-1
#iris <- as.matrix(iris)
#dimnames(iris) <- NULL

iris <- normalize(iris[,1:4])
summary(iris)

# Determine sample size
ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.67,0.33))

# Split the iris data
iris.training <- iris[ind==1, 1:4]
iris.test <- iris[ind==2, 1:4]

# Split the class attribute
iris.trainingtarget <- iris[ind==1, 5]
iris.testtarget <- iris[ind==2, 5]

# One hot encode training target values
iris.trainLabels <- to_categorical(iris.trainingtarget)

# One hot encode test target values
iris.testLabels <- to_categorical(iris.testtarget)

# Print out the iris.testLabels to double check the result
print(iris.testLabels)

