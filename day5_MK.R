read.csv("ModelData_Quarterly.csv")


Modeldata = read.csv("ModelData_Quarterly.csv")
plot(ts(ModelData[,4]))
boxplot(ModelData$Quarter,ModelData$Throughput)
plot(ts(ModelData$Throughput), main = "Differenced Container Throughput Time Series", xlab = "data points", ylab = "throughput")
plot(diff(ts(ModelData$Throughput)))
auto.arima(ModelData$Throughput)
ARIMAFIT = auto.arima(ModelData$Throughput, approximation = FALSE, trace = TRUE, seasonal = TRUE)

ModelData1$Prediction = ARIMAFIT$fitted
ModelData1$Error = ARIMAFIT$residuals
ModelData2 = ModelData1[c(3:4, 7:8)]
colnames(ModelData2)[colnames(ModelData2)=="Year.and.Quarter"]="Period"   
colnames(ModelData2)[colnames(ModelData2)=="Throughput"]="Actual"   
View(ModelData2)

ForecastData = as.data.frame(forecast(ARIMAFIT, h=4, level = 95))

ForecastData$Period = c("2019-Q1", "2019-Q2", "2019-Q3", "2019-Q4"

plot(forecast(ARIMAFIT, h=4, level = 95))

ModelData2[78, 3] = NA

> ModelData2$Period = as.character(ModelData2$Period)
> ModelData2$Error = as.numeric(ModelData2$Error)
> ModelData2$Prediction = as.numeric(ModelData2$Prediction)

> ModelData2$Actual = as.numeric(ModelData2$Actual)
> ModelData2[77, 1] = "2019-Q1"
> View(ModelData2)
> ModelData2[78:80, 1] = c("2019-Q2", "2019-Q3", "2019-Q4")

> x=seq(1,nrow(ModelData2))
> x
[1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
[25] 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48
[49] 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72
[73] 73 74 75 76 77 78 79 80
> plot(x, ModelData2$Actual)
> lines(x, ModelData2$Prediction)
