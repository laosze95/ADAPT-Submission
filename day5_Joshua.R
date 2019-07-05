#read the file
df_Model = read.csv("ModelData_Quarterly.csv")

#select required column
df_Model=df_Model[,1:6]

#install library of various package
library(InformationValue)
library(tseries)
library(forecast)

#Plot a time series for throughput
plot(ts(df_Model$Throughput))
plot(ts(df_Model$Throughput), main = 'Container Throughput Time Series', xlab = 'Data Points', ylab = 'Throughput in Millions')

#plot to check if mean and variance are stationary
plot(diff(ts(df_Model$Throughput)), main = 'Differenced Container Throughout Time Series', xlab = 'Data Points', ylab = '')

#using ARIMA for time series
auto.arima(df_Model$Throughput)
ARIMAfir = auto.arima(df_Model$Throughput, approximation = FALSE, trace = TRUE, seasonal = TRUE)


library(dplyr)

finaltbl = df_Model %>% select(Year.and.Quarter, Throughput)
finaltbl$Prediction = ARIMAfir$fitted

#create new data frame
newdf_Model = cbind(Period = as.character(df_Model$Year.and.Quarter), Actual = ARIMAfir$x, Prediction = ARIMAfir$fitted, Error = ARIMAfir$residuals )
#another method
finaltbl = df_Model %>% select(Year.and.Quarter, Throughput)
finaltbl$Prediction = ARIMAfir$fitted
finaltbl$Error = ARIMAfir$residuals


#forecast for next 4 period
forecast(ARIMAfir$x, h=4, level = 0.95)

#plot the historical and forecast values
plot(forecast(ARIMAfir$x, h=4, level = 0.95))


#write to file
write.csv(newdf_Model, "time_series.csv")