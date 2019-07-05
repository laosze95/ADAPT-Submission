#Modeling Day 5 ADAPT

#Predict the next 4 quarter global port throughput volume in TEU 
#using ARMIA model 

#load data file (set ModelData as name of the file)
ModelData <- read.csv(file="C:/Users/jingmin/Desktop/ADAPT-master/ADAPT-master/ModelData_Quarterly.csv", header=TRUE, sep=",")
View(ModelData)

##If get extra columns, you only need 1 to 6 column, to delete 7 to 9, please do this
ModelData = ModelData[,1:6]
#before comma is for rows, after comma is for columns 

#load library (informationvalue)
library(InformationValue)
library(tseries)
library(forecast)

#explore the dataset 
summary(ModelData)
dim(ModelData)
sum(is.na(ModelData))
class(ModelData)

##plot dataset as time series
library(ggplot2)
#plot time series chart, x axis represents number of data points, y is throughput
#if using ts to plot, make sure your dates data is in order (they do not sort for you)
plot(ts(ModelData$Throughput))

##to add in axis and title 
plot(ts(ModelData$Throughput), main="Differenced Container Throughput Time Series", xlab="Data Points", ylab="Throughput in Millions")

##check if mean and variance are stationary 
#differenced container throughput time series 

plot(ts(diff(ModelData$Throughput)), main="Differenced Container Throughput Time Series", xlab="Data Points", ylab="Throughput in Millions")

##build the auto arima model 
#p - order (number of time lags) of the autoregressive model
#d - degree of differencing (number of times the data have had past values substracted)
#q - order of the moving-average model
ARIMAfit = auto.arima(ModelData$Throughput, approximation = FALSE, trace = TRUE, seasonal = TRUE)

#create the below table to compare actual vs predicted (ERROR)
#table column: Period, Actual, Prediction, Error 

#create new table dataframe
PredictTable <- data.frame(ModelData)

#take just column 3 and 4 as new DF with 2 columns
PredictTable = PredictTable[,3:4]

View(PredictTable)

#changing column names 
colnames(PredictTable) <- c("Period","Actual")

#this contains all the prediction values (76 rows)
ARIMAfit$fitted

#creating 3rd column called prediction and drawing the values from ARIMAfit$fitted (prediction values)
PredictTable$Prediction = ARIMAfit$fitted

#seeing the 3 column names
names(PredictTable)

#creating 4th column called Error
PredictTable$Error = ARIMAfit$residuals

#to view new table
View(PredictTable)

#to know what is ARIMAfit
class(ARIMAfit)
#tells you arimafit is a table with all forecast values inside 

#to see actual values inside ARIMAfit 
ARIMAfit$x

#to plot graph to see if it's good 
plot(PredictTable$Actual,PredictTable$Error)
#it is supposed to be a straight line of values to show it is good 

##forecast the next four quarter using Arima Model along with 95% confidence level
#table names should look like point, forecast, lo95, hi95 with 4 rows 77,78,79,80

#open library
library(forecast)

#create new table (dataframe) from previous table for prediction values column and apply forecast to it
ForecastTable <- data.frame(forecast(PredictTable$Prediction))
View(ForecastTable)

#only want certain columns, remember to use c(1,2,4,5,etc) within for under columns
ForecastTable = ForecastTable[,c(1,4,5)]

#rename columns 
colnames(ForecastTable) <- c("Forecast","Lo 95","Hi 95")

#delete all rows and maintain row 1:4
ForecastTable = ForecastTable[1:4,]

#plotting grap with forecast 
plot(forecast(ARIMAfit, h=4,level=95))

#write the combined historical and forecast file in your working folder

colnames(ForecastTable) <- c("Prediction","Lo 95","Hi 95")

#add 1 column at front
ForecastTable = cbind("Period"=0,ForecastTable)

#add 2nd column at front
ForecastTable = cbind("Actual"=0,ForecastTable)

#change column names
colnames(ForecastTable) <- c("Period","Actual","Prediction","Error")

#change all values in column 4 to 0
ForecastTable$Error = 0

#only retain first 4 columns
ForecastTable = ForecastTable [,1:4]

#need to check and ensure both tables are in same format
str(ForecastTable)
str(PredictionTable)

#if different categories, to change all the num or char or numeric 
ForecastTable$Period =as.factor(ForecastTable$Period)
PredictTable$Prediction = as.numeric(PredictTable$Prediction)
PredictTable$Error = as.numeric(PredictTable$Error)

#combine both files
CombinedTable <- rbind(PredictTable,ForecastTable)

#as factor unable to change row names, so change to character and change the row 77 to 80 column 1 names manually
CombinedTable$Period = as.character(CombinedTable$Period)
CombinedTable[77:80,1] = c("2019-Q1","2019-Q2","2019-Q3","2019-Q4")

##METHOD2:
CombinedTable[77,] <- list("2019-Q1",NA, 198.4892, NA)
CombinedTable[78,] <- list("2019-Q2",NA, 199.9489, NA)
CombinedTable[79,] <- list("2019-Q3",NA, 201.8222, NA)
CombinedTable[80,] <- list("2019-Q4",NA, 203.6956, NA)
View(CombinedTable)

#how to write the file into our harddisk
write.csv(CombinedTable, "C:/Users/jingmin/Desktop/ADAPT-master/Time_Series.csv")
