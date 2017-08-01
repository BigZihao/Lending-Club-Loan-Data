## Analytic Parnters Machine Learining Sample Code
## zihao.zhang@analyticpartners.com
## Sample data are public avaliable in Kaggle: https://www.kaggle.com/benhamner/sf-bay-area-bike-share
## In this example, we illustrate the whole process of data processing, feature engineering, modeling, and parameter tuning
## have fun


##################################################################
##                   Reading data                               ##
##################################################################




trip = read.csv("P:/Lending-Club-Loan-Data/trip.csv")
station =  read.csv("P:/Lending-Club-Loan-Data/station.csv")
weather = read.csv("P:/Lending-Club-Loan-Data/weather.csv")



##################################################################
##                   Data Processing                            ##
##################################################################


names(station)
names(trip)

head(trip)
head(weather)
head(station)

library("lubridate")
trip$start_date = mdy_hm(as.character(trip$start_date))
trip$end_date = mdy_hm(as.character(trip$end_date))


library("dplyr")
library("ggplot2")
trip$date = as.Date(trip$start_date)
trip$end_date = as.Date(trip$end_date)
trip_count = trip %>% group_by(date) %>% summarise(count = n())

##feature engineer, create month, day of week, hour of day.
trip$start_month = month(trip$start_date)
trip$start_wday = wday(as.Date(trip$start_date), label = TRUE)
trip$start_hour = hour(trip$start_date)
trip$end_month = month(trip$end_date)
trip$end_wday = wday(as.Date(trip$end_date), label = TRUE)
trip$end_hour = hour(trip$end_date)
##convert seconds to minutes
trip$duration = trip$duration/60 
trip$is_weekend = ifelse(trip$start_wday %in% c("Sun", "Sat"), 1, 0)
trip$is_weekend = factor(trip$is_weekend, labels = c("weekday", "weekend"))
trip$is_weekend_v2 = ifelse(trip$end_wday %in% c("Sun", "Sat"), 1, 0)
trip$is_weekend_v2 = factor(trip$is_weekend_v2, labels = c("weekday", "weekend"))
##############insight getting##########
trip_date = trip %>% group_by(date) %>% summarise(trip_count = n())
ggplot(trip_date, aes(x = date, y = trip_count)) + geom_point() + geom_smooth(color = "#1A1A1A",method = 'loess') + 
  labs(x = "Date", y = "# of trips", title = "Daily # of Bicylcle Trips from 2013 - 2015") +
  theme(plot.title = element_text(hjust = 0.5))


isweekend_date = trip %>% group_by(date, is_weekend) %>% summarise(count = n())
isweekend_date$is_weekend = factor(isweekend_date$is_weekend, labels = c("weekday", "weekend"))

ggplot(isweekend_date, aes(x = date, y=count)) + geom_point(aes(color = is_weekend), size = 3, alpha = 0.65) +
  labs(x = "Date", y = "Total # of Bicycle Trips") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(isweekend_date, aes(x = date, y=count)) + 
  geom_point() +
  facet_wrap(~ is_weekend) +
  geom_smooth(se = F, method = 'loess') +
  labs(x = "Date", y = "Total # of Bicycle Trips") +
  theme(plot.title = element_text(hjust = 0.5))

hour_format <- c(paste(c(12,1:11),"AM"), paste(c(12,1:11),"PM"))

trip$start_wday <- factor(trip$start_wday)
trip$start_hour <- factor(trip$start_hour, level = 0:23, label = hour_format)

trip_hour_wday = trip %>% group_by(start_wday, start_hour) %>% summarise(count=n())

ggplot(trip_hour_wday, aes(x = start_hour, y = start_wday, fill = count)) + geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6), legend.title = element_blank(), 
        legend.position = "top", legend.direction = "horizontal") +
  labs(x = "Hour of Day", y = "Day of Week of trips", title = "# of bicycle trips ") +
  scale_fill_gradient(low = "orange", high = "black") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(trip, aes(x = date)) + 
  geom_bar(aes(color=subscription_type), stat="count", position = "stack") +
  facet_grid(~is_weekend) +
  labs(x = "Day of Week", y = "# of trips", 
       title = "Customer Vs.Subscriber on Weekend and Weekdays") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank())

station_trip = merge(trip, station, by.x = "start_station_id", by.y = "id")

ggplot(station_trip, aes(x = date)) + 
  geom_bar(aes(color=subscription_type), stat="count", position = "stack") +
  facet_wrap(~city, scales = "free_y") +
  labs(x = "Day of Week", y = "# of trips", 
       title = "Customer Vs.Subscriber by City") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank())


library(plotly)
city_wday = station_trip %>% group_by(start_wday, city) %>% summarise(count = n())
g = ggplot(city_wday, aes(y = city, x = start_wday)) +
  geom_point(aes(size = count, col = count)) +
  scale_size(range = c(1,10)) + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "none")

ggplotly(g, tooltip = c("x", "y", "colour"))


library("ggmap")
bbox = c(-122.4990, 37.31072, -121.7800, 37.88100)
sf = get_map(location = bbox, source = "stamen", maptype = "toner-lite")

map_trip = station_trip %>% group_by(long, lat, zip_code) %>% summarise(count = n())

ggmap(sf) + geom_point(data = map_trip, aes(x = long, y = lat, size = count, color = count)) +
  scale_size(name = "# Total Trips", range = c(3, 12))  + theme(legend.position = "none")



names(station_trip)[22] = "start_lat"
names(station_trip)[23] = "start_long"
end_station_trip = merge(trip, station, by.x = "end_station_id", by.y = "id")
names(end_station_trip)[22] = "end_lat"
names(end_station_trip)[23] = "end_long"


road_df = merge(station_trip, end_station_trip, by = "id") %>% 
  select ("id","start_lat", "start_long", "end_lat", "end_long", "city.y", "city.x") %>% 
  filter(city.y == "San Francisco" & city.x == "San Francisco")

road_map = road_df %>% group_by(start_lat, start_long, end_lat, end_long) %>% summarise(num_trips = n())
station_sf = station %>% filter(city=="San Francisco")


library("ggrepel")
ggplot(road_map) + geom_segment(aes(x=start_long, xend = end_long, y = start_lat, yend=end_lat, 
                                    size = num_trips, colour = num_trips, alpha = num_trips)) +
  geom_point(data = station_sf, aes(x=long, y=lat), size = 4) +
  geom_text_repel(data = station_sf, aes(x=long, y=lat, label=name), size = 4) +
  theme_light(base_size = 10) +
  scale_colour_gradient(low ="#132B43", high = "#56B1F7", limits=c(0, max(road_map$num_trips)), name="Number of Trips") +
  scale_alpha(limits=c(0, max(road_map$num_trips)), guide = F) +
  scale_size(limits=c(0, max(road_map$num_trips)), guide = F) +
  xlab("") + ylab("") + coord_fixed() +
  theme(axis.line = element_blank(), 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank())


weather = weather[weather$zip_code == 94107,]

weather$date = mdy(as.character(weather$date))
#set nan value as "Normal" in the event 
levels(weather$events) = c(levels(weather$events), "Normal")
weather$events[weather$events==''] = "Normal"
weather$events[weather$events=='rain'] = "Rain"
droplevels(weather, except = c("Normal", "Fog", "Rain", "Fog-Rain", "Rain-Thunderstorm"))

weather$precipitation_inches = as.numeric(as.matrix(weather$precipitation_inches))

weather = weather %>% group_by(date) %>% mutate(median = median(precipitation_inches, na.rm=T))
weather$precipitation_inches = ifelse(is.na(weather$precipitation_inches), weather$median, weather$precipitation_inches)
weather$precipitation_inches[is.na(weather$precipitation_inches)] = 0
summary(weather)



weather = weather %>% group_by(max_wind_Speed_mph) %>% mutate(gust_median = median(max_gust_speed_mph, na.rm=T))
weather$max_gust_speed_mph = ifelse(is.na(weather$max_gust_speed_mph), weather$gust_median, weather$max_gust_speed_mph)


##feature engineer, create month, day of week, dummay for weekend days, seasonal variable

weather$month = month(weather$date)
weather$wday = wday(weather$date)
weather$weekend = ifelse(weather$wday %in% c(6,7), 1, 0)

weather$season = weather$month
weather$season[weather$season==12] = 2
weather$season[weather$season==1] = 2
weather$season[weather$season==3] = 5
weather$season[weather$season==4] = 5
weather$season[weather$season==6] = 8
weather$season[weather$season==7] = 8
weather$season[weather$season==9] = 11
weather$season[weather$season==10] = 11
weather$season = factor(weather$season, labels = c("Sum", "Aut", "Win", "Spr"))


library("timeDate")

listHolidays()

Holiday = c(
  as.Date(USChristmasDay(2013)),
  as.Date(USColumbusDay(2013)),
  as.Date(USCPulaskisBirthday(2013)),
  as.Date(USDecorationMemorialDay(2013)),
  as.Date(USElectionDay(2013)),
  as.Date(USGoodFriday(2013)),
  as.Date(USInaugurationDay(2013)),
  as.Date(USIndependenceDay(2013)),
  as.Date(USLaborDay(2013)),
  as.Date(USLincolnsBirthday(2013)),
  as.Date(USMemorialDay(2013)),
  as.Date(USMLKingsBirthday(2013)),
  as.Date(USNewYearsDay(2013)),
  as.Date(USPresidentsDay(2013)),
  as.Date(USThanksgivingDay(2013)),
  as.Date(USVeteransDay(2013)),
  as.Date(USWashingtonsBirthday(2013)),
  as.Date(USChristmasDay(2014)),
  as.Date(USColumbusDay(2014)),
  as.Date(USCPulaskisBirthday(2014)),
  as.Date(USDecorationMemorialDay(2014)),
  as.Date(USElectionDay(2014)),
  as.Date(USGoodFriday(2014)),
  as.Date(USInaugurationDay(2014)),
  as.Date(USIndependenceDay(2014)),
  as.Date(USLaborDay(2014)),
  as.Date(USLincolnsBirthday(2014)),
  as.Date(USMemorialDay(2014)),
  as.Date(USMLKingsBirthday(2014)),
  as.Date(USNewYearsDay(2014)),
  as.Date(USPresidentsDay(2014)),
  as.Date(USThanksgivingDay(2014)),
  as.Date(USVeteransDay(2014)),
  as.Date(USWashingtonsBirthday(2014)),
  as.Date(USChristmasDay(2015)),
  as.Date(USColumbusDay(2015)),
  as.Date(USCPulaskisBirthday(2015)),
  as.Date(USDecorationMemorialDay(2015)),
  as.Date(USElectionDay(2015)),
  as.Date(USGoodFriday(2015)),
  as.Date(USInaugurationDay(2015)),
  as.Date(USIndependenceDay(2015)),
  as.Date(USLaborDay(2015)),
  as.Date(USLincolnsBirthday(2015)),
  as.Date(USMemorialDay(2015)),
  as.Date(USMLKingsBirthday(2015)),
  as.Date(USNewYearsDay(2015)),
  as.Date(USPresidentsDay(2015)),
  as.Date(USThanksgivingDay(2015)),
  as.Date(USVeteransDay(2015)),
  as.Date(USWashingtonsBirthday(2015))
)

weather$isholiday = ifelse(weather$date %in% Holiday, 1, 0)

#####add number of trips by day into weather data frome for further analysis.
df = merge(trip, weather, by = "date")[, -c(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 16, 17, 18, 20, 43, 44, 46, 47, 48)]


trip_num = df %>% group_by(date, start_hour) %>% summarise(count = n())

df_v2 = merge(df, trip_num, by = c("date", "start_hour"))

names(df_v2)[2] = "hour"
names(df_v2)[4] = "month"
names(df_v2)[5] = "wday"

df_v2$month <- as.factor(df_v2$month)
library(dummies)
df_v2$is_weekend <- as.numeric(df_v2$is_weekend)
df_v2$isholiday <- as.numeric(df_v2$isholiday)
df_v2 <- dummy.data.frame(df_v2, dummy.class="factor")

dim(df_v2)
head(df_v2)
summary(df_v2)


###    Taking sample of the total dataset to reduce running time


use_sample = sample(nrow(df_v2), nrow(df_v2)*0.001)
df_v2 = df_v2[use_sample,]
dim(df_v2)

df_v2$count <- as.numeric(df_v2$count)

train_sample = sample(nrow(df_v2), nrow(df_v2)*0.8)



##################################################################
##                   Random Forest                              ##
##################################################################




#install.packages("randomForest")
library("randomForest")
library("reshape2")


train_data = data.frame(df_v2[train_sample, -1])
test_data = data.frame(df_v2[-train_sample, -1])
train_data.y = df_v2$count[train_sample]
test_data.y = df_v2$count[-train_sample]

dim(train_data)
names(train_data)
dim(test_data)
length(train_data.y)
length(test_data.y)


rf.df <- data.frame(trees = c(1:50),0,0,0)
j=1

for(i in c(10,15,20)){
  j=j+1
  for (k in c(1:50)){
  best_model = randomForest(count~., data = train_data , ntree=k, mtry=i)
  Predicted_Value = predict(best_model, newdata = test_data)
  
  rf.df[k,j] = sqrt(mean((Predicted_Value-test_data.y)^2))
 # print(c(k,i,j,rf.df[k,j]))
  }
}



error_rf = rf.df
names(error_rf) = c("ntree", "depth_10", "depth_15", "depth_20")
MSE_matrix_rf = melt(error_rf, id="ntree")

ggplot(MSE_matrix_rf, aes(x=ntree, y=value, colour = variable)) + geom_line() +
  labs(x="number of trees", y="Test MSE")




Predicted_Value = predict(best_model, newdata = test_data)
sqrt(mean((Predicted_Value-test_data.y)^2))




a = data.frame(df_v2[-train_sample, c('date','count')], Predicted_Value)
names(a) = c("date", "Actual", "Prediction")

AP = melt(a, id = "date")
AP$variable= as.factor(AP$variabl)
ggplot(AP, aes(x = date, y = value, colour = variable)) + geom_line() +
  labs(x = "Date", y = "number of trips", title = "Predicted Values vs Actual Values") +
  theme(plot.title = element_text(hjust = 0.5))


##################################################################
##                   XGboost model                              ##
##################################################################

#install.packages("xgboost")
require(xgboost)
ix <- which( names(df_v2)  %in% c("date","count")) 
train_data = data.frame(df_v2[train_sample, -ix])
test_data = data.frame(df_v2[-train_sample, -ix])
train_data.y = df_v2$count[train_sample]
test_data.y = df_v2$count[-train_sample]

dtrain <- xgb.DMatrix(data = data.matrix(train_data), label=data.matrix(train_data.y))
dtest <- xgb.DMatrix(data = data.matrix(test_data), label=data.matrix(test_data.y))

watchlist <- list(eval = dtest, train = dtrain)
boost.df <- data.frame(trees = c(1:50))
j=1
for(i in c(10,15,20)){
bst <- xgb.train( data = dtrain,
                max_depth=i,
                eta = 0.1,
                subsample = 0.7,
                colsample_bytree = 0.7,
                nthread = 8, 
                nrounds=50, 
                watchlist = watchlist,
                eval_metric = "rmse",
                objective = "reg:linear")
j=j+1
boost.df[,j] <-bst$evaluation_log$eval_rmse
}

names(bst)
bst$evaluation_log$eval_rmse

model <- xgb.dump(bst , with.stats = T)
model[1:10] #This statement prints top 10 nodes of the model

names <- dimnames(data.matrix(train_data))[[2]]

importance_matrix <- xgb.importance(names, model = bst)
# 制图
xgb.plot.importance(importance_matrix[1:20,])




error_boosting = boost.df
names(error_boosting) = c("ntree", "depth_10", "depth_15", "depth_20")
MSE_matrix = melt(error_boosting, id="ntree")

ggplot(MSE_matrix, aes(x=ntree, y=value, colour = variable)) + geom_line() +
  labs(x="number of trees", y="Test MSE")


bst <- xgb.train( data = dtrain,
                  max_depth=20,
                  eta = 0.1,
                  subsample = 0.7,
                  colsample_bytree = 0.7,
                  nthread = 8, 
                  nrounds=50, 
                  watchlist = watchlist,
                  eval_metric = "rmse",
                  objective = "reg:linear")

yhat.boost = predict(bst, dtest)

sqrt(mean((yhat.boost-test_data.y)^2))

boost.a = data.frame(df_v2[-train_sample, c('date','count')], yhat.boost)
names(boost.a) = c("date", "Actual", "Prediction")

boost.AP = melt(boost.a, id = "date")
boost.AP$variable= as.factor(boost.AP$variable)
ggplot(boost.AP, aes(x = date, y = value, colour = variable)) + geom_line() +
  labs(x = "Date", y = "number of trips", title = "Predicted Values vs Actual Values") +
  theme(plot.title = element_text(hjust = 0.5))



