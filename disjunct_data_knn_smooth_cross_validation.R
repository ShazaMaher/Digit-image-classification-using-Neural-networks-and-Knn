require(caret)
graphics.off()
par("mar")
par(mar=c(1,1,1,1))
#install.packages("class")
library("class")
#install.packages("gmodels")
library(gmodels)
#install.packages("lattice")
library(lattice)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("caret")
library(caret)
library(spatstat)
library(RSNNS)
set.seed(42365)

time_many=numeric()
accurcy_many = numeric()
normalize <- function(x) {  
  return ((x - min(x)) / (max(x) - min(x))) 
}
data = load("C:/Users/Shaza.DESKTOP-JTENLV4/Desktop/statistic-maskinelearning/Data/idList-pre-100.Rdata")

# data pre 100 dpi
# disjunct train set

# read train data 70% from whole data = 55,3 ~~ 55
id_pre_100_disjunct_train <- do.call(rbind, idList[1:55]) 
id_pre_100_disjunct_train <- as.data.frame(id_pre_100_disjunct_train) 
id_pre_100_disjunct_train[,1] <- factor(id_pre_100_disjunct_train[,1])
round(prop.table(table(id_pre_100_disjunct_train$V1)) * 100, digits = 1)
# normalazation
id_pre_100_disjunct_train_sn <- as.data.frame(lapply(id_pre_100_disjunct_train[,-1], normalize))
dim(id_pre_100_disjunct_train_sn)
train_labels_disjunct = id_pre_100_disjunct_train[,1]


# read test data
id_pre_100_disjunct_test_input <- do.call(rbind, idList[56:71]) 
id_pre_100_disjunct_test_input <- as.data.frame(id_pre_100_disjunct_test_input) 
id_pre_100_disjunct_test_input[,1] <- factor(id_pre_100_disjunct_test_input[,1])
round(prop.table(table(id_pre_100_disjunct_test_input$V1)) * 100, digits = 1)

id_pre_100_disjunct_test <- do.call(rbind, idList[72:79]) 
id_pre_100_disjunct_test <- as.data.frame(id_pre_100_disjunct_test) 
id_pre_100_disjunct_test[,1] <- factor(id_pre_100_disjunct_test[,1])
round(prop.table(table(id_pre_100_disjunct_test$V1)) * 100, digits = 1)
# normalazation
id_pre_100_disjunct_test_input_sn <- as.data.frame(lapply(id_pre_100_disjunct_test_input[,-1], normalize))
id_pre_100_disjunct_test_sn <- as.data.frame(lapply(id_pre_100_disjunct_test[,-1], normalize))
dim(id_pre_100_disjunct_test_sn)

test_labels_disjunct = id_pre_100_disjunct_test[,1]
# applying knn with 70% of the data and choosin k as odd nummer of the squrt root of 70% of the data
startTime =proc.time()
id_Knn_pred_disjunct <- knn(id_pre_100_disjunct_train_sn,id_pre_100_disjunct_test_sn,train_labels_disjunct,331)
endTime = proc.time()
time =endTime - startTime

cf_dis<-CrossTable(x=test_labels_disjunct,y=id_Knn_pred_disjunct, prop.chisq=FALSE)
accurcy= sum(diag(cf_dis$t))/sum(cf_dis$t)
plot(cf_dis$t)


##########################3
#Apply ANN
lev_train <- levels(id_pre_100_disjunct_train$V1) # Number of classes

nnTrainingClass <- matrix(nrow = length(id_pre_100_disjunct_train$V1), ncol = 10, data = 0) # Create a list probabilities, for all labels

for(i in 1:length(id_pre_100_disjunct_train$V1)) { # Set probabilities to one for matching class
  matchList <- match(lev_train,toString(id_pre_100_disjunct_train$V1[i]))
  matchList[is.na(matchList)] <- 0
  nnTrainingClass[i,] <- matchList
}

trainingClass <- as.data.frame(nnTrainingClass)
startTime = proc.time()
model_200_0 <- mlp(id_pre_100_disjunct_train_sn, trainingClass, maxit = 200,
       initFunc = "Randomize_Weights", learnFunc = "Std_Backpropagation", learnFuncParams = c(0.1),inputsTest = id_pre_100_disjunct_test_input, targetsTest = id_pre_100_disjunct_test_input[,1], 
       linOut = FALSE)
endTime = proc.time()
time_model_200_0 =endTime - startTime

plot(model_200_0)
model
weightMatrix(model)
extractNetInfo(model)



plotIterativeError(model_200_0)

predictions_200_0 <- predict(model_200_0,id_pre_100_disjunct_test_sn)

plotRegressionError(predictions_200_0[,1], id_pre_100_disjunct_test[,1], pch = 10) 
plotROC(model_200_0$fitted.values[,1], id_pre_100_disjunct_train[,1]) 
plotROC(predictions_200_2[,2], id_pre_100_disjunct_test[,1])

plotIterativeError(model_200_2) 
plotRegressionError(id_pre_100_disjunct_train[,1], model_200_2$fitted.values) 
plotRegressionError(id_pre_100_disjunct_test[,1], model_200_2$fittedTestValues) 
R> hist(model$fitted.values - patterns$targetsTrain)


#Using the "predict" function we have recieved "predictions"
responselist <- matrix(nrow = length(predictions[,1]), ncol = 1, data = "Na")

for(i in 1:nrow(predictions)) {
  responselist[i,] <- toString( which(predictions[i,]==max(predictions[i,])) - 1 )
}
responselist <- data.frame(responselist)
responselist[,1] <- as.factor(responselist[,1])

# Calculating the accuracy
agreement_rbf <- responselist[,1] == id_pre_100_disjunct_test[,1]
table(agreement_rbf)
prop.table(table(agreement_rbf))

plotRegressionError(predictions[,6], id_pre_100_disjunct_test[,1])

targets_fitted_vales <- confusionMatrix(id_pre_100_disjunct_train[,1],fitted.values(model))
targets_predictions_200_0 <- confusionMatrix(predictions_200_0,id_pre_100_disjunct_test[,1])
accurcy_model_200_0= sum(diag(targets_predictions_200_0))/sum(targets_predictions_200_0)
plot(targets_predictions_200_0)

plotROC(fitted.values(model)[,2], id_pre_100_disjunct_train[,2])
plotROC(predictions[,2], id_pre_100_disjunct_test[,2])

#confusion matrix with 402040-method
confusion_matrix_with_402040_method <-confusionMatrix(id_pre_100_disjunct_train[,1], encodeClassLabels(fitted.values(model),
                                                     method="402040", l=0.4, h=0.6))


#############################

#########################
#doing performance for different k:
x <- c(99, 199, 299, 301, 355)
#fitting models for 30 different k-values()
for( k in x[1:5]){
  startTime =proc.time()
  id_Knn_pred_disjunct_many <- knn(id_pre_100_disjunct_train_sn,id_pre_100_disjunct_test_sn,train_labels_disjunct,k)
  endTime = proc.time()
  totalTime =endTime - startTime
  time_many[k]=totalTime;
  cf_many<-CrossTable(x=test_labels_disjunct,y=id_Knn_pred_disjunct_many, prop.chisq=FALSE)
  accurcy_many[k]= sum(diag(cf_many$t))/sum(cf_many$t)
}

# PLOTTING
par(mfrow=c(2,2))
par(mfrow=c(2,2))
plot(c(99,199,299,301,355),time_many_5, col='red', type='b', xlab = "the applied K", ylab = "consumed time")
plot(c(99,199,299,301,355),accurcy_many_5, col='blue', type='b',xlab = "the applied K", ylab = "accuracy")
plot(c(99,199,299,301,355),time_many_5, col='red', type='b', xlab = "the applied K", ylab = "consumed time")

#########################

######################################
#cross validation:
folds <- createFolds(cf$t, k=10)
str(folds)
timeCV = numeric()
accurcyCV = numeric()


for(k in 1:100){
  for(i in 1:10)
  {
    
    # Split the data in train and test
    folded_train <- id_shuffled[-folds[[i]],-1]
    folded_test <- id_shuffled[folds[[i]],-1]
    folded_train_labels <- id_shuffled[-folds[[i]],1]
    folded_test_labels <- id_shuffled[folds[[i]],1]}
  startTime =proc.time()
  fold_Knn_pred <- knn(folded_train,folded_test,folded_train_labels,k)
  endTime = proc.time()
  totalTime =endTime - startTime
  timeCV[k] <- totalTime
  cfV1 <-CrossTable(x=folded_test_labels,y=fold_Knn_pred, prop.chisq=FALSE)
  cfV[k] <- list(cfV1)
  
  #accurcy
  
  accurcyCV[k] = sum(diag(cfV[[k]]$t))/sum(cfV[[k]]$t)
  
  
  
}

#Plotting
plot(1:k, timeCV, col='red', type='b')
plot(1:k,accurcyCV, col='blue', type='b')

######################################


id_pre_sn_pca_100 <- prcomp(id_pre_sn_100,center = TRUE,scale. = TRUE)
########################
#Showing image before smoothing:
id_mat_show <- data.matrix(id_pre_100, rownames.force = NA)
id_mat_sn_show <- data.matrix(id_pre_sn_pca_100$x, rownames.force = NA)

rotate <- function(x) t(apply(x, 2, rev))
# Show first 10 images
for(i in 1:10)
{
  rotated <- c(id_mat_sn_show[-200+i*200+1,2:ncol(id_mat_sn_show)])
  rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
  image <- matrix(rotated,nrow = imageSize,ncol = imageSize, byrow = FALSE)
  image <- rotate(image)
  image( image,  zlim=c(0,1), col=gray(0:100/100) )
}



# smooth images
id_mat_train_s <- data.matrix(id_pre_100_disjunct_train, rownames.force = NA)
id_mat_test_s <- data.matrix(id_pre_100_disjunct_test, rownames.force = NA)

imageSize <- sqrt(ncol(id_mat_test_s)-1)

rotate <- function(x)t(apply(x,2,rev))

smoothImage <-function(grayImg){
  
  smoothed <- as.matrix(blur(as.im(grayImg),sigma = 4,
                             normalise = FALSE,bleed = TRUE,
                             varcov=NULL))
  
  return(smoothed)
}
# Smooth all images
for(i in 1:nrow(id_mat_test_s))
{
  rotated <- c(id_mat_test_s[i,2:ncol(id_pre_100_disjunct_test)])
  image <- matrix(rotated,nrow = imageSize,ncol = imageSize, byrow = FALSE)
  image <- smoothImage(image)
  id_mat_test_s[i,2:ncol(id_mat_test_s)] <- matrix(image,nrow = 1,ncol = ncol(id_mat_test_s) - 1, byrow = FALSE)
}

#Showing image after smothing:
id_mat_test_smooth <- data.matrix(id_mat_test_s, rownames.force = NA)

rotate <- function(x) t(apply(x, 2, rev))

# Show first 10 images
for(i in 1:10)
{
  rotated <- c(id_mat_test_smooth[-500+i*500+1,2:ncol(id_mat_test_smooth)])
  rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
  image <- matrix(rotated,nrow = imageSize,ncol = imageSize, byrow = FALSE)
  image <- rotate(image)
  image( image,  zlim=c(0,1), col=gray(0:255/255) )
}

id_train_s <- as.data.frame(id_mat_train_s)
id_test_s <- as.data.frame(id_mat_test_s)
id_train_s[,1] <- factor(id_train_s[,1])
id_test_s[,1] <- factor(id_test_s[,1])
round(prop.table(table(id_train_s$V1)) * 100, digits = 1)
round(prop.table(table(id_test_s$V1)) * 100, digits = 1)

id_sn <- as.data.frame(lapply(id_train_s[-1], normalize))

#apply knn on smooth images
# split the data into train and test set where all persons in 50 - 50:
train_all_person_in_smooth = id_sn[1:79000,]
test_all_person_in_smooth = id_sn[79001:158000,]
train_labels_all_person_in_smooth = id[1:79000,1]
test__labels_all_person_in_smooth = id[79001:158000,1]
startTime = proc.time()
id_Knn_pred_all_persons_in_smooth <- knn(train_all_person_in_smooth,test_all_person_in_smooth,train_labels_all_person_in_smooth,281)
endTime = proc.time()
time_knn_smooth =endTime - startTime
cf_smooth<-CrossTable(x=test__labels_all_person_in_smooth,y=id_Knn_pred_all_persons_in_smooth, prop.chisq=FALSE)
accurcy_smooth= sum(diag(cf_smooth$t))/sum(cf_smooth$t)
library(caret)
install.packages("C50", dependencies = TRUE)
library(C50)
install.packages("irr", dependencies = TRUE)
library(lpSolve)
library(irr)
#cross validation:
train_set <- createDataPartition(classes, p = 0.9, list = FALSE)
str(train_set)

folds <- createFolds(train_set, k=10)
str(folds)

timeCV = numeric()
accurcyCV = numeric()
for(k in 1:10)
for(i in 1:10)
{
  # Split the data in train and test
  folded_train <- id_[-folds[[i]],]
  folded_test <- id_sn[folds[[i]],]
  folded_train_labels <- id[-folds[[i]],1]
  folded_test_labels <- id[folds[[i]],1]
  
  startTime =proc.time()
  fold_Knn_pred <- knn(folded_train,folded_test,folded_train_labels,331)
  endTime = proc.time()
  totalTime =endTime - startTime
  timeCV[i] <- totalTime
  cfV <-CrossTable(x=folded_test_labels,y=fold_Knn_pred, prop.chisq=FALSE)
  #accurcy
  accurcyCV[i] = sum(diag(cfV$t))/sum(cfV$t)
}}


#Plotting
plot(c(1:10), timeCV, col='red', type='b',
     xlab="1 to 10 folds",
     ylab="time used to train Knn with K = 281 ")
plot(c(1:10),accurcyCV, col='blue', type='b')
id_pca_smooth <- prcomp(id_sn,center = TRUE,scale. = TRUE)

#Showing image after smothing:
id_mat_smooth <- data.matrix(id_pca_smooth$x, rownames.force = NA)

rotate <- function(x) t(apply(x, 2, rev))
# Show first 10 images
for(i in 1:10)
{
  rotated <- c(id_mat_smooth[-400+i*400+1,2:ncol(id_mat_smooth)])
  rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
  image <- matrix(rotated,nrow = imageSize,ncol = imageSize, byrow = FALSE)
  image <- rotate(image)
  image( image,  zlim=c(0,1), col=gray(0:255/255) )
}
