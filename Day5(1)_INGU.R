
getwd()
ModelData <- read.csv("C:/Users/user/Documents/Day2/ModelData_Quarterly.CSV");

## delete last 3 column,no data.
ModelData=ModelData[,1:6]

library(InformationValue)
library(tseries)
library(forecast)

str(ModelData)
summary(ModelData)
class(ModelData)
head(ModelData,10)
tail(ModelData,10)
sample(ModelData,3)
dim(ModelData)

plot(ModelData$Year,ModelData$Throughput)

## ts function for time-series
plot(ts(ModelData$Throughput))
ts(ModelData$Throughput)
class(ts(ModelData$Throughput))
plot(ts(ModelData$Throughput), main = "Differenced Container Throughput Time Series",xlab="Data Points",ylab="Throughput in Millions")
## Difference between 2 data
plot(diff(ts(ModelData$Throughput)),main = "Differenced Container Throughput Time Series",xlab="Data Points", ylab="" )
## function diff <-> cumsum check
plot(cumsum(ts(ModelData$Throughput)),main = "Differenced Container Throughput Time Series",xlab="Data Points", ylab="" )
## Use ARIMA Model
auto.arima(ModelData$Throughput,approximation = FALSE, trace = TRUE, seasonal = TRUE)

New_ModelData <- data.frame(ModelData$Year.and.Quarter, ModelData$Throughput) 

Model_arima <- auto.arima(ModelData$Throughput)

ModelFinal <- data.frame(ModelData$Year.and.Quarter, Model_arima$x,Model_arima$fitted, Model_arima$fitted - Model_arima$x)
names(ModelFinal) <- c("Period","Actual","Prediction","Error")

## Use of forecast function with 95% confidence Level
ModelFcast <- forecast(Model_arima, h = 4, level = 0.95)
ModelFcast
plot(ModelFcast)

ModelFcast <- data.frame(forecast(Model_arima, h = 4, level = 0.95))
names(ModelFcast) <- c("Prediction","Lo95","H95")

ModelFinal[77,] <- list("2019-Q1",NA,"198.4892",NA)
ModelFinal[78,] <- list("2019-Q2",NA,"203.9078",NA)
ModelFinal[79,] <- list("2019-Q3",NA,"204.7478",NA)
ModelFinal[80,] <- list("2019-Q4",NA,"203.3269",NA)

## Write to the File in Folder
write.csv(ModelFinal,"C:/Users/user/Documents/Day2/tiem_series.csv")


