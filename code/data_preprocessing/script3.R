#import rgdal package
#install.packages("rgdal")
#library(rgdal)
#install.packages("tree")
#library(tree)
#install.packages("MultivariateRandomForest")
#library(MultivariateRandomForest)
#install.packages("randomForest")
#library(randomForest)

### filter data to 2016 only
trainUTM = read.csv("C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\trainUTM.csv",as.is = TRUE)
testUTM = read.csv("C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\testUTM.csv",as.is = TRUE)
allData = rbind(trainUTM,testUTM)
ind2016 = (allData$year == 2016)
data2016 = allData[ind2016,]
data2016 = data2016[-c(which(is.na(data2016$year))),]
# work only on suscribers that have legal zipcode
data2016 = data2016[data2016$user_type == 1,]
data2016 = data2016[,-c(1,8,16)]
data2016 = data2016[!is.na(data2016$zipcode),]
#write.csv(data2016, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\dataSuscribers2016_4RT.csv")
write.csv(data2016, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\dataSuscribers2016_4RT1.csv")

### split to trainSet & testSet
set.seed(7)
data2016_random_order <- data2016[sample(nrow(data2016)), ]
train_2016_size <- floor((nrow(data2016)/4)*3)
trainSuscribers_2016 <- data2016_random_order[1:train_2016_size, ]
testSuscribers_2016 <- data2016_random_order[(train_2016_size+1):nrow(data2016), ]
write.csv(trainSuscribers_2016, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\trainSuscribers2016_4RT1.csv")
write.csv(testSuscribers_2016, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\testSuscribers2016_4RT1.csv")
#write.csv(trainSuscribers_2016, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\trainSuscribers2016_4RT.csv")
#write.csv(testSuscribers_2016, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\testSuscribers2016_4RT.csv")

# define train & test for each coordinate - X or Y
train = read.csv("C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\trainSuscribers2016_4RT.csv", as.is = TRUE)
test = read.csv("C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\testSuscribers2016_4RT.csv", as.is = TRUE)
#train = trainSuscribers_2016
#test = testSuscribers_2016

#reduce size of datasets
set.seed(1)
train = train[sample(150000),]
test = test[sample(50000),]

#split to trainX & trainY
RT.trainX = train[-c(1,5,15,16,18)]
RT.trainY = train[-c(1,5,15,16,17)]
RT.test = test[-c(1,5,15,16,17,18)]

### Regression Trees Predictions
#install.packages("tree")
library(tree)
# model learning on Train data - X coordinate (East-West)
RT.X = tree(X_end ~ . , data = RT.trainX)
plot(RT.X)
text(RT.X, pretty = 0, cex=0.5)
summary(RT.X)

# predict the X-axis value for the test data and compute test error (in meters)
prediction.RTx = predict(RT.X,RT.test)
predictionErr.RTx = test$X_end - prediction.RTx
RSS = sum(predictionErr.RTx ^2)
MSE <- RSS/length(predictionErr.RTx)
RMSE.RTx <- sqrt(MSE)
RMSE.RTx

# model learning on Train data - Y coordinate (East-West)
RT.Y = tree(Y_end ~ . , data = RT.trainY)
plot(RT.Y)
text(RT.Y, pretty = 0, cex=0.5)
summary(RT.Y)

# predict the Y-axis value for the test data and compute test error (in meters)
prediction.RTy = predict(RT.Y,RT.test)
predictionErr.RTy = test$Y_end - prediction.RTy
RSS = sum(predictionErr.RTy ^2)
MSE <- RSS/length(predictionErr.RTy)
RMSE.RTy <- sqrt(MSE)
RMSE.RTy

# Model Evaluation - End-stations locations (XY coordinates)
dist_2D = sqrt((predictionErr.RTx)^2 +(predictionErr.RTy)^2)
RSS = sum(dist_2D^2)
MSE <- RSS/length(dist_2D)
RMSE.RTxy <- sqrt(MSE)
RMSE.RTxy


### Random Forest Predictions
#install.packages("randomForest")
library(randomForest)
library(caret)
library(doParallel)
cores = 3
cl = makePSOCKcluster(cores)
registerDoParallel(cl)
# model learning on Train data - X coordinate (East-West)
RF.X = randomForest(X_end ~ . , data = RT.trainX, ntree = 50, do.trace=TRUE, method = "parRF")
save(RF.X,file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\RF.X.RData")
#load("C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\RF.X.RData")
RF.X
varImpPlot(RF.X)

# model evaluation on Test data - X coordinate (East-West)
prediction.RFx <- predict(RF.X,RT.test)
summary(prediction.RFx)
predictionErr.RFx = test$X_end - prediction.RFx
RSS = sum(predictionErr.RFx ^2)
MSE <- RSS/length(predictionErr.RFx)
RMSE.RFx <- sqrt(MSE)
RMSE.RFx

# model learning on Train data - Y coordinate (East-West)
RF.Y = randomForest(Y_end ~ . , data = RT.trainY, ntree = 100,do.trace=TRUE, method = "parRF")
save(RF.Y,file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\RF.Y.RData")
#load("C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\RF.Y.RData")
RF.Y
varImpPlot(RF.Y)

# model evaluation on Test data - X coordinate (East-West)
prediction.RFy <- predict(RF.Y,RT.test)
summary(prediction.RFy)
predictionErr.RFy = test$Y_end - prediction.RFy
RSS = sum(predictionErr.RFy ^2)
MSE <- RSS/length(predictionErr.RFy)
RMSE.RFy <- sqrt(MSE)
RMSE.RFy

# model Evaluation - End-stations locations (XY coordinates)
dist_2D = sqrt((predictionErr.RFx)^2 +(predictionErr.RFy)^2)
RSS = sum(dist_2D^2)
MSE <- RSS/length(dist_2D)
RMSE.RFxy <- sqrt(MSE)
RMSE.RFxy

# KNN
library(class)
#create improved test data with predicted coordinates
predicted_test = data.frame(RT.test,0,prediction.RFx,prediction.RFy)
#apply KNN with based on all features and predict end station ID
train.knn = train[,-c(16)]
test.knn = predicted_test[,-c(16)]
trainClass.knn = train[,16]
knn.pred = knn(train.knn,test.knn,trainClass.knn,k = 1)
for (i in 1:nrow(predicted_test)) {
  predicted_test$prediction.KNNx[i] = UTMs[which(UTMs$station_id == knn.pred[i]),2]
  predicted_test$prediction.KNNy[i] = UTMs[which(UTMs$station_id == knn.pred[i]),3]
}
#apply KNN based on X & Y only and predict end station ID
train.knn = UTMs[,c(2,3)]
test.knn = predicted_test[,c(16,17)]
trainClass.knn = UTMs[,1]
knn.pred = knn(train.knn,test.knn,trainClass.knn,1)
for (i in 1:nrow(predicted_test)) {
  predicted_test$prediction.KNNx[i] = UTMs[which(UTMs$station_id == knn.pred[i]),2]
  predicted_test$prediction.KNNy[i] = UTMs[which(UTMs$station_id == knn.pred[i]),3]
}
#compute X error
xErr.knn = test$X_end - predicted_test$prediction.KNNx
RSS = sum(xErr.knn ^2)
MSE <- RSS/length(xErr.knn)
RMSE.knn <- sqrt(MSE)
RMSE.knn
#compute Y error
yErr.knn = test$Y_end - predicted_test$prediction.KNNy
RSS = sum(yErr.knn ^2)
MSE <- RSS/length(yErr.knn)
RMSE.knn <- sqrt(MSE)
RMSE.knn
# model Evaluation - End-stations locations (XY coordinates)
dist_2D = sqrt((xErr.knn)^2 +(yErr.knn)^2)
RSS = sum(dist_2D^2)
MSE <- RSS/length(dist_2D)
RMSE.KNNxy <- sqrt(MSE)
RMSE.KNNxy
#compute KNN performance
knn.table = table(knn.pred,test[,16])
#Accuracy
knn.accuracy = sum(diag(knn.table))/sum(knn.table)
knn.accuracy
#Misclassification
knn.misClassification = 1 - knn.accuracy
knn.misClassification

#TODO FROM HERE  
dist_2D_knn = sqrt((xErr.knn)^2 +(yErr.knn)^2)
dist_2D_rf = sqrt((predictionErr.RFx)^2 +(predictionErr.RFy)^2)

bestKNN =  dist_2D_knn < dist_2D_rf
predicted_test$bestX[bestKNN] = predicted_test$prediction.KNNx[bestKNN]
predicted_test$bestY[bestKNN] = predicted_test$prediction.KNNy[bestKNN]

predicted_test$bestX[!bestKNN] = predicted_test$prediction.RFx[!bestKNN]
predicted_test$bestY[!bestKNN] = predicted_test$prediction.RFy[!bestKNN]
#compute X error
xErr.best = test$X_end - predicted_test$bestX
RSS = sum(xErr.best ^2)
MSE <- RSS/length(xErr.best)
RMSE.bestX <- sqrt(MSE)
RMSE.bestX
#compute Y error
yErr.best = test$Y_end - predicted_test$bestY
RSS = sum(yErr.best ^2)
MSE <- RSS/length(yErr.best)
RMSE.bestY <- sqrt(MSE)
RMSE.bestY
# model Evaluation - End-stations locations (XY coordinates)
dist_2D = sqrt((xErr.best)^2 +(yErr.best)^2)
RSS = sum(dist_2D^2)
MSE <- RSS/length(dist_2D)
RMSE.BESTxy <- sqrt(MSE)
RMSE.BESTxy




# K-means

#create improved test data with predicted coordinates
predicted_test_kmeans = data.frame(RT.test,0,prediction.RFx,prediction.RFy)
colnames(predicted_test_kmeans)[c(13,14,15)] = colnames(train)[c(16,17,18)]

for (i in 1:10000){
  data4km = rbind(train[,c(17,18)],predicted_test_kmeans[i,c(17,18)])
  km = kmeans(data4km,13)
  correctedXY_ind = tail(1:nrow(data4km),1) 
  predicted_test_kmeans$correctedX[i] = km$centers[km$cluster[correctedXY_ind],1]
  predicted_test_kmeans$correctedY[i] = km$centers[km$cluster[correctedXY_ind],2]
  print(i)
}
#compute X error
xErr.kmeans = test$X_end[1:10000] - predicted_test_kmeans$correctedX[1:10000]
RSS = sum(xErr.kmeans ^2)
MSE = RSS/length(xErr.kmeans)
RMSE.kmX = sqrt(MSE)
RMSE.kmX
#compute Y error
yErr.kmeans = test$Y_end[1:10000] - predicted_test_kmeans$correctedY[1:10000]
RSS = sum(yErr.kmeans ^2)
MSE = RSS/length(yErr.kmeans)
RMSE.kmY = sqrt(MSE)
RMSE.kmY
# model Evaluation - End-stations locations (XY coordinates)
dist_2D = sqrt((xErr.kmeans)^2 +(yErr.kmeans)^2)
RSS = sum(dist_2D^2)
MSE <- RSS/length(dist_2D)
RMSE.KMxy <- sqrt(MSE)
RMSE.KMxy


data4km = rbind(train[,c(17,18)],predicted_test_kmeans[,c(14,15)])
km = kmeans(data4km,15)
cl = km$cluster
cl = as.data.frame(cl)
centers = as.data.frame(km$centers)
#plot(data4km,col=(km$cluster+1),pch=20,cex=2)
#clusplot(data4km,km$cluster,color=TRUE,shade=TRUE,col.p = km$cluster)
k = 1
for (i in (150001:200000)){
  predicted_test_kmeans$correctedX[k] = km$centers[km$cluster[i],1]
  predicted_test_kmeans$correctedY[k] = km$centers[km$cluster[i],2]
  k = k + 1
}




data4km = rbind(train[,c(17,18)],predicted_test[1,c(17,18)])
#km = kmeans(train[,c(17,18)],37)
km = kmeans(data4km,20,20)
cl = km$cluster
cl = as.data.frame(cl)
centers = as.data.frame(km$centers)
plot(data4km,col=(km$cluster+1),pch=20,cex=2)


#### learn MVT model & get Prediction in 2D
#install.packages("MultivariateRandomForest")
library(MultivariateRandomForest)
train_inputs_mat = as.matrix(train[c(1:1000),-c(1,5,15,16,17,18)])
train_outputs_mat = as.matrix(train[c(1:1000),c(17,18)])
test_inputs_mat = as.matrix(test[c(1:250),-c(1,5,15,16,17,18)])
mvrPredict = build_forest_predict(train_inputs_mat,train_outputs_mat,50,5,5,test_inputs_mat)
str(as.data.frame(mvrPredict))
save(mvrPredict,file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\mvrPredict.RData")
# compute test error
predictionErr.MVT = sqrt((test$X_end[c(1:250)] - mvrPredict[,1])^2 + (test$Y_end[c(1:250)] - mvrPredict[,2])^2)
RSS = sum(predictionErr.MVT ^2)
MSE <- RSS/length(predictionErr.MVT)
RMSE.MVT <- sqrt(MSE)


RF.X1000 = randomForest(X_end ~ . , data = RT.trainX[c(1:1000),], ntree = 50, do.trace=TRUE)
save(RF.X,file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\RF.X.RData")
#load("C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\RF.X.RData")
RF.X
varImpPlot(RF.X)

# model evaluation on Test data - X coordinate (East-West)
prediction.RFx1000 <- predict(RF.X1000,RT.test[c(1:250),])
summary(prediction.RFx1000)
predictionErr.RFx1000 = test$X_end[c(1:250)] - prediction.RFx1000
RSS = sum(predictionErr.RFx1000 ^2)
MSE <- RSS/length(predictionErr.RFx1000)
RMSE.RFx1000 <- sqrt(MSE)
RMSE.RFx1000

# model learning on Train data - Y coordinate (East-West)
RF.Y1000 = randomForest(Y_end ~ . , data = RT.trainY[c(1:1000),], ntree = 50,do.trace=TRUE)
save(RF.Y,file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\RF.Y.RData")
#load("C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\RF.Y.RData")
RF.Y
varImpPlot(RF.Y)

# model evaluation on Test data - X coordinate (East-West)
prediction.RFy1000 <- predict(RF.Y1000,RT.test[c(1:250),])
summary(prediction.RFy1000)
predictionErr.RFy1000 = test$Y_end[c(1:250)] - prediction.RFy1000
RSS = sum(predictionErr.RFy1000 ^2)
MSE <- RSS/length(predictionErr.RFy1000)
RMSE.RFy1000 <- sqrt(MSE)
RMSE.RFy1000

# model Evaluation - End-stations locations (XY coordinates)
dist_2D = sqrt((predictionErr.RFx1000)^2 +(predictionErr.RFy1000)^2)
RSS = sum(dist_2D^2)
MSE <- RSS/length(dist_2D)
RMSE.RFxy1000 <- sqrt(MSE)
RMSE.RFxy1000

#install.packages("tree")
library(tree)
# model learning on Train data - X coordinate (East-West)
RT.X1000 = tree(X_end ~ . , data = RT.trainX[c(1:1000),])
plot(RT.X)
text(RT.X, pretty = 0, cex=0.5)
summary(RT.X)

# predict the X-axis value for the test data and compute test error (in meters)
prediction.RTx1000 = predict(RT.X1000,RT.test[c(1:250),])
predictionErr.RTx1000 = test$X_end[c(1:250)] - prediction.RTx1000
RSS = sum(predictionErr.RTx1000 ^2)
MSE <- RSS/length(predictionErr.RTx1000)
RMSE.RTx1000 <- sqrt(MSE)
RMSE.RTx1000

# model learning on Train data - Y coordinate (East-West)
RT.Y1000 = tree(Y_end ~ . , data = RT.trainY[c(1:1000),])
plot(RT.Y)
text(RT.Y, pretty = 0, cex=0.5)
summary(RT.Y)

# predict the Y-axis value for the test data and compute test error (in meters)
prediction.RTy1000 = predict(RT.Y1000,RT.test[c(1:250),])
predictionErr.RTy1000 = test$Y_end[c(1:250)] - prediction.RTy1000
RSS = sum(predictionErr.RTy1000 ^2)
MSE <- RSS/length(predictionErr.RTy1000)
RMSE.RTy1000 <- sqrt(MSE)
RMSE.RTy1000

# Model Evaluation - End-stations locations (XY coordinates)
dist_2D = sqrt((prediction.RTx1000)^2 +(prediction.RTy1000)^2)
RSS = sum(dist_2D^2)
MSE <- RSS/length(dist_2D)
RMSE.RTxy1000 <- sqrt(MSE)
RMSE.RTxy1000




### PCA
pc = prcomp(data2016[-c(16,17,18)], center = TRUE, scale. = TRUE) 