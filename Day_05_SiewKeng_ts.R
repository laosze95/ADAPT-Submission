library(dplyr)
library(readxl)
library(ggplot2)
library(reshape2)
library(stringr)
library(lubridate)
library(data.table)
library(InformationValue)
library(tseries)
library(forecast)


setwd("D:/ADAPT")
df_Data = data.frame()


path="D:/ADAPT"
#load ADAPT_Sample_dataset*.csv into df_Data 
myfiles = dir(path)

for (file in myfiles)
{
  if(grepl("csv",file))
  {
    indata<- fread(file,check.names = FALSE, stringsAsFactors = FALSE)
    df_Data<-rbind(df_Data,indata)
  }
  
}

ModelData<-df_Data[,1:6]
ModelData$QoQ_Growth_n <-as.numeric(gsub("%", "", ModelData$`QoQ Growth`))
ModelData$YoY_Growth_n <-as.numeric(gsub("%", "", ModelData$`YoY Growth`))

#EDA
str(ModelData)
sapply(ModelData, class)
summary(ModelData)


plot(as.numeric(as.factor(ModelData$`Year and Quarter`)),ModelData$Throughput)
plot(as.numeric(as.factor(ModelData$`Year and Quarter`)),as.numeric((gsub("%", "", ModelData$`QoQ Growth`))))
plot(ModelData$Year,ModelData$Throughput)
plot(ModelData$Throughput)
plot(ts(ModelData$Throughput))


#timeseries
ts(ModelData$Throughput)

plot(ts(diff(ModelData$Throughput)))


#arima model
ARIMAfit = auto.arima(ModelData$Throughput, approximation = FALSE, trace = TRUE, seasonal = TRUE)


#model result
ModelResult<-df_Data[,1:4]
ModelResult$ARIMAfit_fitted <-ARIMAfit$fitted
ModelResult$ARIMAfit_residuals <-ARIMAfit$residuals

plot(ts(ModelResult$Throughput,ModelResult$ARIMAfit_fitted))


#model forecast
df_Forecast<-data.frame(forecast(ARIMAfit,model=fit,level = 95, h = 4))
fcast <-forecast(ARIMAfit,model=fit,level = 95, h = 4)
plot(fcast)


##combine model with forecast, output to file
df_Forecast$'Year and Quarter'<-c('2019-Q1','2019-Q2','2019-Q3','2019-Q4')

df_Result <- rbind(data.frame('Year_Quarter' = ModelResult$'Year and Quarter',
                              'Actual_Throughput' = as.numeric(ModelResult$"Throughput"),
                              'Prediction' = as.numeric(ModelResult$"ARIMAfit_fitted"),
                              'Error' = as.numeric(ModelResult$"ARIMAfit_residuals"))
                   ,data.frame('Year_Quarter' = df_Forecast$'Year and Quarter',
                               'Actual_Throughput' = 0,
                               'Prediction' = as.numeric(df_Forecast$'Point.Forecast'),
                               'Error' =  0))

write.csv(df_Result,"time_series.csv")


                                      