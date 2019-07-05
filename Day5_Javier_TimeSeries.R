#Javier Kan Day 5 Exercise
library(dplyr)
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(forecast)
library(GGally)
library(PerformanceAnalytics)
library(psych)
library(reshape2)
library(stringr)
library(tseries)
library(InformationValue)
library(data.table)

#Set working directory
setwd("C:/Users/javierk/Desktop/Admin/Main/2019/ADAPT/ADAPT Practical")
getwd()
Path = "C:/Users/javierk/Desktop/Admin/Main/2019/ADAPT/ADAPT Practical"
Path

#Assign path 
filenames = dir(Path)

#Create new dataframe
df_Data = data.frame()

#For each file check for .csv extension
for(file in filenames){
  if(grepl(".csv",file,ignore.case = TRUE))
  {
    #Read csv file
    temp=read.csv(file)
    #Append data
    df_Data = rbind(df_Data,temp)
  }
  
}

View(df_Data)

#Explore dataset
str(df_Data)
dim(df_Data)
colSums(is.na(df_Data))

#Time Series
ggplot(data = df_Data, aes(x = df_Data$Year, y = df_Data$Throughput)) + 
geom_line(color = "#FC4E07", size = 2)

#Alternate way for Time Series
plot(ts(df_Data[,4]))
#show time series data
(ts(df_Data[,4]))

#check if mean and variance are stationary
tsseriesdiff <- diff(df_Data$Throughput, differences=1)
plot.ts(tsseriesdiff)

#Arima model
arimaModel <- auto.arima(df_Data$Throughput, approximation = FALSE, trace = TRUE, seasonal = TRUE)
arimaModel

#Actual
arimaModel$x
#Predicted
arimaModel$fitted

#Create table to show actual vs prediction  
cbinded_df<-cbind(as.character(df_Data$Year.and.Quarter),arimaModel$x,arimaModel$fitted,arimaModel$x-arimaModel$fitted)
View(cbinded_df)

ggplot(data = df_Data, aes(x = arimaModel$x, y = arimaModel$fitted)) + 
geom_line(color = "#FC4E07", size = 2)

#Plot forecast chart and table
plot(forecast(auto.arima(df_Data$Throughput)))
forecastdf<-data.frame(forecast(auto.arima(df_Data$Throughput)))
View(forecastdf)
forecast95 <- forecastdf[c(1,4,5)]
View(forecast95)
#Alternate way to plot
fcast <- forecast(arimaModel, h=4, level =95)
plot(fcast)

#Combined old and forecast table
newpredicted <- cbind("New Q","NA",fcast$fitted,"NA")
combinedTable <- rbind(cbinded_df,newpredicted)
View(combinedTable)

#Write into CSV
write.csv(combinedTable,"Outputresult.csv")
