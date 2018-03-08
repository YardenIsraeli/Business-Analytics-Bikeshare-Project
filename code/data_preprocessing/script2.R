#import rgdal package
#install.packages("rgdal")
#library(rgdal)
#install.packages("tree")
#library(tree)
#install.packages("MultivariateRandomForest")
#library(MultivariateRandomForest)

### load train & test
train4tree_withPrediction_num = read.csv(file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\train4tree_withPrediction_num.csv", as.is = TRUE)
train = train4tree_withPrediction_num

test4tree_withPrediction_num = read.csv(file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\test4tree_withPrediction_num.csv", as.is = TRUE)
test = test4tree_withPrediction_num

### convert end locations from GEO to UTM 
bike_stations = read.csv('C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\dataSets\\san_francisco_data\\sanFran_stations.csv');

#create data frame to hold UTM coordinates for each station
UTMs = data.frame(station_id = bike_stations$station_id,
                  X = vector(mode = "numeric", length = nrow(bike_stations)),
                  Y = vector(mode = "numeric", length = nrow(bike_stations)))

for (i in 1:nrow(bike_stations)){
  bike_geoCoords = data.frame(lon = bike_stations$longitude[i],lat = bike_stations$latitude[i]) #longitude first
  coordinates(bike_geoCoords) = c("lon","lat")
  bike_geoCoords = SpatialPoints(bike_geoCoords,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  bike_utmCoords = spTransform(bike_geoCoords, CRS("+proj=utm +zone=10 ellps=WGS84"))
  bike_utmCoords = as.data.frame(bike_utmCoords)
  colnames(bike_utmCoords) = c("X","Y")
  
  UTMs$X[i] = bike_utmCoords$X
  UTMs$Y[i] = bike_utmCoords$Y
}
### merge train data with UTM coordinates
train = merge(train,UTMs,by.x = c("end_station_id"),by.y = c("station_id"),all = TRUE)
trainUTM = data.frame(train$trip_id,train$start_station_id,train$start_lat,train$start_lon,
                                       train$duration_minutes,train$start_dockcount,train$year,train$month_num,
                                       train$day_status_num,train$hour,train$minDist_bart,
                                       train$minDist_caltrain,train$minDist_ferry,train$minDist_muni,
                                       train$user_type_num,train$zipcode,train$bike_id,
                                       train$end_station_id,train$X.y,train$Y)
colnames(trainUTM) = c("trip_id","start_station_id","start_lat","start_lon",
                                        "duration_minutes","start_dockcount","year","month",
                                        "day_status","hour","minDist_bart","minDist_caltrain","minDist_ferry",
                                        "minDist_muni","user_type","zipcode","bike_id","end_station_id",
                                        "X_end","Y_end")

write.csv(trainUTM, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\trainUTM.csv")

### merge test data with UTM coordinates
test = merge(test,UTMs,by.x = c("end_station_id"),by.y = c("station_id"),all = TRUE)
testUTM = data.frame(test$trip_id,test$start_station_id,test$start_lat,test$start_lon,
                     test$duration_minutes,test$start_dockcount,test$year,test$month_num,
                     test$day_status_num,test$hour,test$minDist_bart,
                     test$minDist_caltrain,test$minDist_ferry,test$minDist_muni,
                     test$user_type_num,test$zipcode,test$bike_id,
                     test$end_station_id,test$X.y,test$Y)
colnames(testUTM) = c("trip_id","start_station_id","start_lat","start_lon",
                       "duration_minutes","start_dockcount","year","month",
                       "day_status","hour","minDist_bart","minDist_caltrain","minDist_ferry",
                       "minDist_muni","user_type","zipcode","bike_id","end_station_id",
                       "X_end","Y_end")

write.csv(testUTM, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\testUTM.csv")