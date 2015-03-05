
library(lubridate)
library(tseries)
library(forecast)
library(zoo)
library(astsa)
library(plyr)
require(lubridate)
cities = list("City1","City2","City3","City4","City5")
#  cities=list("City3")
length(cities)
item=1
for (item in 1:length(cities))
{
  filename = paste0("./CAX_DemandForecasting_Training_Data/CAX_DemandForecasting_",cities[item],"_Train.csv")
  data = read.csv(filename)
  data$day = substr(data$Date,1,2)
  data$month = substr(data$Date,4,5)
  data$year = substr(data$Date,7,10)
  data$mytimes <- with(data, ISOdatetime(year,month,day,0,0,0))
  data$week=weekdays(data$mytimes)
  head(data,n=14)
 
# Aggregating over weekday of each month
  aggreg=aggregate(data$AGGREGATE.DEMAND,list(mon=data$month,wk=data$week),mean)
  names(aggreg) =c("month","week","Agg.Demand")
  aggreg$Agg.Demand = ceiling(aggreg$Agg.Demand)
  aggreg$month = as.numeric(aggreg$month)
  
  ## MAPE Calculation on Training Data ### 
  map = data[c("mytimes","day","month","year","AGGREGATE.DEMAND","week")]
  head(map)
  map$month = as.numeric(map$month)
  
  actual_map <- join(map,aggreg, by = c("month","week"))
  head(actual_map)
  actual_map$MAPE  = (actual_map$AGGREGATE.DEMAND - actual_map$Agg.Demand)/actual_map$AGGREGATE.DEMAND
  print(" MAPE on Training Data")
  print (mean(actual_map$MAPE))
  
  ## Forecasting 
  foreca=seq(as.Date("2013/1/1"), by = "day", length.out = 365)
  foreca = data.frame(foreca)
  head(foreca)
  foreca$month = month(foreca$foreca)
  foreca$week = weekdays(foreca$foreca)
  fore_cast <- join(foreca, aggreg, by = c("month","week"))
  head(fore_cast)
  plot(fore_cast$Agg.Demand,type='l')
  output=paste0(filename,"output-final.csv")
  write.csv(fore_cast,output)
}
