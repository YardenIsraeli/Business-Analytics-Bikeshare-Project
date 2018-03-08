######### data4reg.R in Rscripts ###########

#import rgdal package
#install.packages("rgdal")
library(rgdal)

#bike stations in san-francisco 
bike_stations = read.csv('C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\dataSets\\san_francisco_data\\sanFran_stations.csv');

#bart interest stations in san francisco - East,Notrh
bart_10_utm = c(X = 551592.135306969,Y = 4181537.8863182277) #bart_10_geo = c(37.779732,	-122.414123)
bart_19_utm = c(X = 553088.8311343134,Y = 4183005.558662792) #bart_19_geo = c(37.792874,	-122.39702)
bart_29_utm = c(X = 552735.0669643998,Y = 4182618.384535158) #bart_29_geo = c(37.789405, -122.401066)
bart_36_utm = c(X = 552130.290420729,Y = 4182067.085771976)  #bart_36_geo = c(37.784471,	-122.407974)

#call-train interest stations in san francisco - East,Notrh
caltrain_utm = c(X = 553296.6586609936,Y = 4181218.3196450667) #call_train_geo = c(37.776754,-122.394791)

#ferry-boats interest stations in san francisco - East,Notrh
ferry1_utm = c(X = 553408.0042776837,Y = 4183291.00093222)   #ferry1_geo = c(37.795428, -122.393374)
ferry2_utm = c(X = 553920.8758899837,Y = 4181407.7870783666) #ferry2_geo = c(37.778425, -122.387689)

#muni interest stations in san francisco - East,Notrh
munni_utm = c(X = 553432.2772469815,Y = 4182657.7198097403)  #munni_geo = c(37.789719, -122.393145)

#create data frame to hold minimum distances for each station
min_dists = data.frame(station_id = bike_stations$station_id,
                       minDist_bart = vector(mode = "numeric", length = nrow(bike_stations)),
                       minDist_caltrain = vector(mode = "numeric", length = nrow(bike_stations)),
                       minDist_ferry = vector(mode = "numeric", length = nrow(bike_stations)),
                       minDist_muni = vector(mode = "numeric", length = nrow(bike_stations)))
#compute the minimum distance of the i'th bike station to interest stations
for (i in 1:nrow(bike_stations)){
  bike_geoCoords = data.frame(lon = bike_stations$longitude[i],lat = bike_stations$latitude[i]) #longitude first
  coordinates(bike_geoCoords) = c("lon","lat")
  bike_geoCoords = SpatialPoints(bike_geoCoords,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  bike_utmCoords = spTransform(bike_geoCoords, CRS("+proj=utm +zone=10 ellps=WGS84"))
  bike_utmCoords = as.data.frame(bike_utmCoords)
  colnames(bike_utmCoords) = c("X","Y")
  
  #compute minimum distance from bart
  dist_bart_10 = sqrt(sum((bike_utmCoords-bart_10_utm)^2))
  dist_bart_19 = sqrt(sum((bike_utmCoords-bart_19_utm)^2))
  dist_bart_29 = sqrt(sum((bike_utmCoords-bart_29_utm)^2))
  dist_bart_36 = sqrt(sum((bike_utmCoords-bart_36_utm)^2))
  min_dists$minDist_bart[i] = min(c(dist_bart_10,dist_bart_19,dist_bart_29,dist_bart_36))
  
  #compute minimum distance from caltrain
  dist_caltrain = sqrt(sum((bike_utmCoords-caltrain_utm)^2))
  min_dists$minDist_caltrain[i] = dist_caltrain
  
  #compute minimum distance from ferry
  dist_ferry1 = sqrt(sum((bike_utmCoords-ferry1_utm)^2))
  dist_ferry2 = sqrt(sum((bike_utmCoords-ferry2_utm)^2))
  min_dists$minDist_ferry[i] = min(c(dist_ferry1,dist_ferry2))
  
  #compute minimum distance from muni
  dist_muni = sqrt(sum((bike_utmCoords-munni_utm)^2))
  min_dists$minDist_muni[i] = dist_muni
}
train = read.csv('C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\dataSets\\san_francisco_data\\sanFran_train_extended.csv', as.is = TRUE) 
train$zip_code = as.numeric(train$zip_code)
train = merge(train,min_dists,by.x = c("start_station_id"),by.y = c("station_id"),all = TRUE)

train4tree_withPrediction = data.frame(train$trip_id,train$start_station_id,train$start_lat,train$start_lon,
                                       train$duration_minutes,train$start_dockcount,train$year,train$Month,
                                       train$Weekday,train$dayStatus,train$Hour,train$minDist_bart,
                                       train$minDist_caltrain,train$minDist_ferry,train$minDist_muni,
                                       train$subscriber_type,train$zip_code,train$bike_number,
                                       train$end_station_id,train$end_lat,train$end_lon,train$end_dockcount)
colnames(train4tree_withPrediction) = c("trip_id","start_station_id","start_lat","start_lon",
                                        "duration_minutes","start_dockcount","year","month","weekday",
                                        "day_status","hour","minDist_bart","minDist_caltrain","minDist_ferry",
                                        "minDist_muni","user_type","zipcode","bike_id","end_station_id",
                                        "end_lat","end_lon","end_dockcount")
#train4tree = train4tree_withPrediction[1:18] 
#save data for trees
write.csv(train4tree_withPrediction, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\train4tree_withPrediction.csv")
#write.csv(train4tree, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\train4tree.csv")
#save test data with numeric converter
train4tree_withPrediction$month_num = match(train4tree_withPrediction$month,month.name)
train4tree_withPrediction$day_status_num = ifelse(train4tree_withPrediction$day_status %in% c("weekday"),1,0)
train4tree_withPrediction$user_type_num = ifelse(train4tree_withPrediction$user_type %in% c("Subscriber"),1,0)
train4tree_withPrediction_num = train4tree_withPrediction[-c(8,9,10,16,22)]
write.csv(train4tree_withPrediction_num,"C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\train4tree_withPrediction_num.csv")

test = read.csv('C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\dataSets\\san_francisco_data\\sanFran_test_extended.csv') 
test$zip_code = as.numeric(test$zip_code)
test = merge(test,min_dists,by.x = c("start_station_id"),by.y = c("station_id"),all = TRUE)
test4tree_withPrediction = data.frame(test$trip_id,test$start_station_id,test$start_lat,test$start_lon,
                                      test$duration_minutes,test$start_dockcount,test$year,test$Month,
                                      test$Weekday,test$dayStatus,test$Hour,test$minDist_bart,
                                      test$minDist_caltrain,test$minDist_ferry,test$minDist_muni,
                                      test$subscriber_type,test$zip_code,test$bike_number,
                                      test$end_station_id,test$end_lat,test$end_lon,test$end_dockcount)

colnames(test4tree_withPrediction) = c("trip_id","start_station_id","start_lat","start_lon",
                                       "duration_minutes","start_dockcount","year","month","weekday",
                                       "day_status","hour","minDist_bart","minDist_caltrain","minDist_ferry",
                                       "minDist_muni","user_type","zipcode","bike_id","end_station_id",
                                       "end_lat","end_lon","end_dockcount")
#test4tree = train4tree_withPrediction[1:18] 
#save data for trees
write.csv(test4tree_withPrediction, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\test4tree_withPrediction.csv")
#write.csv(test4tree, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\dataSets\\san_francisco_data\\test4tree.csv")
#save test data with numeric converter
test4tree_withPrediction$month_num = match(test4tree_withPrediction$month,month.name)
test4tree_withPrediction$day_status_num = ifelse(test4tree_withPrediction$day_status %in% c("weekday"),1,0)
test4tree_withPrediction$user_type_num = ifelse(test4tree_withPrediction$user_type %in% c("Subscriber"),1,0)
test4tree_withPrediction_num = test4tree_withPrediction[-c(8,9,10,16,22)]
write.csv(test4tree_withPrediction_num,"C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\scriptsFlowR\\dataSets\\test4tree_withPrediction_num.csv")