
library(lubridate)
library(tseries)
library(forecast)
library(zoo)
library(astsa)
library(plyr)
require(lubridate)

cities = list("City1","City2","City3","City4","City5")
# cities=list("City1")
length(cities)
item=1
for (item in 1:length(cities))
{
  filename = paste0("./CAX_DemandForecasting_Training_Data/CAX_DemandForecasting_",cities[item],"_Train.csv")
  data = read.csv(filename)
  data[is.na(data)] = 0
  mat = data[-c(1,2,3)]
  mat$sl = 1:nrow(mat)
  mat$ln =log(mat$AGGREGATE.DEMAND) 
  plot(mat$ln,type='l')
  library(forecast)
  model=lm(ln~log(sl)+cos(2*pi*sl*2/14)+sin(2*pi*sl*2/14),data=mat)
  summary(model)
  mat$fitted = data.frame(model$fitted)
  head(mat)
  
  mat$pred.demand= exp(mat$fitted)
  
  mat$MAPE  = (mat$AGGREGATE.DEMAND - mat$pred.demand)/mat$AGGREGATE.DEMAND
  print("MAPE value on training data")
  print(sum(mat$MAPE)/nrow(mat))
  
  n1= data.frame(seq(732,732+(365*1)))
  names(n1) ="sl"
  n1$predi = predict(model,n1)
  
  n1$deman = (exp(n1$predi))
  n1$deman = floor(n1$deman )
  plot(n1$deman,type='l')
  
  ## Working on Residuals
  z = model$residuals
  Box.test(z)
  tsz = ts(z,freq=7)
  plot(tsz)
  k=auto.arima(tsz)
  summary(k)
  Box.test(k$residuals)
  forec=forecast(k,h=366)
  n1$residlnfore = forec$mean
  head(n1)
  n1$resid = exp(n1$residlnfore)
  n1$finaldeamnd= floor(n1$deman+n1$resid)
  plot(n1$finaldeamnd)
  ##
  output= paste0(cities[item],".csv")
  write.csv(n1,output)
}

