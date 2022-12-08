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
set.seed(42365)

time =numeric()
accurcy = numeric()
normalize <- function(x) {  
  return ((x - min(x)) / (max(x) - min(x))) 
}
data = load("C:/Users/Shaza.DESKTOP-JTENLV4/Desktop/statistic-maskinelearning/Data/idList-pre-100.Rdata")

# data pre 100 dpi
# all person in train set
id_pre_100 <- do.call(rbind, idList) 
id_pre_100 <- as.data.frame(id_pre_100) 
id_pre_100[,1] <- factor(id_pre_100[,1])
round(prop.table(table(id_pre_100$V1)) * 100, digits = 1)
# normalazation
id_pre_sn_100 <- as.data.frame(lapply(id_pre_100[-1], normalize))
dim(id_pre_sn_100)
# split the data into train and test set where all persons in 50 - 50:
train_all_person_in = id_pre_sn_100[1:79000,]
test_all_person_in = id_pre_sn_100[79001:158000,]
train_labels_all_person_in = id_pre_100[1:79000,1]
test__labels_all_person_in = id_pre_100[79001:158000,1]
#id_Knn_pred_all_persons_in <- knn(train_all_person_in,test_all_person_in,train_labels_all_person_in,281)

#########################
#doing performance for different k:
x <- c(100, 150, 200, 250, 300)
#fitting models for 30 different k-values()
for( k in x[1:5]){
  startTime =proc.time()
  id_Knn_pred <- knn(train_all_person_in,test_all_person_in,train_labels_all_person_in,k)
  endTime = proc.time()
  totalTime =endTime - startTime
  time[k]=totalTime;
  cf<-CrossTable(x=test__labels_all_person_in,y=id_Knn_pred, prop.chisq=FALSE)
  accurcy[k]= sum(diag(cf$t))/sum(cf$t)
}

# PLOTTING
plot(c(100,150,200,250,300), c(time[100], time[150],time[200],time[250],time[300]), col='red', type='b')
plot(c(100,150,200,250,300), c(accurcy[100], accurcy[150],accurcy[200],accurcy[250],accurcy[300]), col='blue', type='b')
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
id_mat <- data.matrix(id_pre_100, rownames.force = NA)

imageSize <- sqrt(ncol(id_mat)-1)

rotate <- function(x)t(apply(x,2,rev))

smoothImage <-function(grayImg){
  
  smoothed <- as.matrix(blur(as.im(grayImg),sigma = 0.5,
                             normalise = FALSE,bleed = TRUE,
                             varcov=NULL))
  
  return(smoothed)
}
# Smooth all images
for(i in 1:nrow(id_mat))
{
  rotated <- c(id_mat[i,2:ncol(id_pre_100)])
  image <- matrix(rotated,nrow = imageSize,ncol = imageSize, byrow = FALSE)
  image <- smoothImage(image)
  id_mat[i,2:ncol(id_mat)] <- matrix(image,nrow = 1,ncol = ncol(id_mat) - 1, byrow = FALSE)
}

#Showing image after smothing:
id_mat_smooth <- data.matrix(id_mat, rownames.force = NA)

rotate <- function(x) t(apply(x, 2, rev))

# Show first 10 images
for(i in 1:10)
{
  rotated <- c(id_mat_smooth[-600+i*600+1,2:ncol(id_mat_smooth)])
  rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
  image <- matrix(rotated,nrow = imageSize,ncol = imageSize, byrow = FALSE)
  image <- rotate(image)
  image( image,  zlim=c(0,1), col=gray(0:255/255) )
}

id <- as.data.frame(id_mat)
id[,1] <- factor(id[,1])
round(prop.table(table(id$V1)) * 100, digits = 1)
id_sn <- as.data.frame(lapply(id[-1], normalize))

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

for(i in 1:10)
  {
    # Split the data in train and test
    folded_train <- id_sn[-folds[[i]],]
    folded_test <- id_sn[folds[[i]],]
    folded_train_labels <- id[-folds[[i]],1]
    folded_test_labels <- id[folds[[i]],1]
    
   startTime =proc.time()
   fold_Knn_pred <- knn(folded_train,folded_test,folded_train_labels,281)
   endTime = proc.time()
   totalTime =endTime - startTime
   timeCV[i] <- totalTime
   cfV <-CrossTable(x=folded_test_labels,y=fold_Knn_pred, prop.chisq=FALSE)
  #accurcy
  accurcyCV[i] = sum(diag(cfV$t))/sum(cfV$t)
}


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
