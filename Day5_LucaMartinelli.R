install.packages(
  c(
    "ggplot2",
    "GGally",
    "Psych",
    "Tseries",
    "forecast",
    "InformationValue",
    "PerformanceAnalytics",
    "readxl",
    "dplyr",
    "reshape2",
    "stringr",
    "lubridate",
    "readr",
    "ggplot",
    "car",
    "tseries",
    "forecast"
  )
)


library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)
library(stringr)
library(lubridate)
library(tseries)
library(forecast)


setwd('C:/Users/martinelli/Documents/ADAPT PROGRAM/DAY 5/workspace/')

# Load every csv files 

path = 'C:/Users/martinelli/Documents/ADAPT PROGRAM/DAY 5/workspace'
ModelData = read.csv(paste(path,'ModelData_Quarterly.csv',sep='/'),check.names = FALSE)
ModelData = ModelData[,1:6]

colnames(ModelData)


class(ModelData)
str(ModelData)
summary(ModelData)

#Missing data columns
sapply(ModelData, function(x) sum(is.na(x)))

#Check if data are numerics
unlist(lapply(mtcars, is.numeric))

ts(ModelData[,4])
# To test the type
class(ts(ModelData[,4]))

line(ModelData$Throughput)
boxplot(ModelData$Throughput)

plot(ts(ModelData[,4]))

plot(ts(ModelData[,4]),main="Differenced Container Throughput Time Series", xlab='Data Points', ylab='Throughput')

# CHeck if the mean and the variance are stationary
# Difference Container throuput time series
# plotting differenced data to see if the mean and cariance are stationary for ARIMA
plot(diff(ts(ModelData[,4])),main="Differenced Container Throughput Time Series", xlab='Data Points', ylab='Throughput')

auto.arima(ModelData[,4], seasonal = FALSE)

# Usign AUTO ARIMA
ARIMAfit= auto.arima(ModelData$Throughput,approximation=FALSE, trace=TRUE, seasonal=FALSE)

# Create a new Below table to compare Actual vs Predicted (Error)

# New 
ModelDataPrediction = data.frame("Period"=ModelData$`Year and Quarter`,
                                 "Actual"=ModelData$Throughput,
                                 "Prediction"="",
                                 "Error"="")


ModelDataPrediction$Prediction = ARIMAfit$fitted
ModelDataPrediction$Error = ARIMAfit$residuals

#plot the actual and the prediction

plot(diff(ts(ModelDataPrediction[,4])),main="Differenced Container Throughput Time Series", xlab='Data Points', ylab='Throughput')

#Plotting di errors
plot(ts(ModelDataPrediction[,4]))
#Plotting the actual
plot(ts(ModelDataPrediction[,2]))

str(ModelDataPrediction)
ts(ModelDataPrediction[,4])

#Forecast the Next Four Quarter Using ARIMA MODEL ALONG WITH 95% Confidence level
forecastData = forecast(ARIMAfit,level=95,h=4)

ts(forecastData)
plot(forecastData)

forecastedData = data.frame(forecastData$fitted)

# Merge the ModelDataPrediction to Forecast Data
str(ModelDataPrediction)
ModelDataPrediction$Period=as.character(ModelDataPrediction$Period)
ModelDataPrediction$Prediction=as.numeric(ModelDataPrediction$Prediction)
ModelDataPrediction$Error=as.numeric(ModelDataPrediction$Error)

ModelDataPrediction[77,] <- list("2019-Q1" , NA, 198.4892, NA)
ModelDataPrediction[78,] <- list("2019-Q2" , NA, 203.9078, NA)
ModelDataPrediction[79,] <- list("2019-Q3" , NA, 204.7478, NA)
ModelDataPrediction[80,] <- list("2019-Q4" , NA, 203.3269, NA)

# write in a file
write.csv(ModelDataPrediction,"C:/Users/martinelli/Documents/ADAPT PROGRAM/DAY 5/time_series.csv")

plot(ts(ModelDataPrediction[,3]))
