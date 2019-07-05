#Set the working path directory
filenames = list.files("C:/Users/zhangdj\Desktop/DataForExcellence/Workshop_Dataset/Day5")

#Creating dataframe for csv files
df_Data_CSV = data.frame()

for(file in filenames)
{
  if(grepl(".csv",file,ignore.case = TRUE))
  {
    temp=read.csv(file)
    df_Data_CSV = rbind(df_Data_CSV,temp)
  }
}

library(InformationValue)
library(InformationValue)
library(tseries)
library(ggplot2)
library(forecast)

dim(df_Data) 
str(df_Data)
names(df_Data)
sapply(df_Data, class)
summary(df_Data)

plot(ts(df_Data[,4]))
ts(df_Data[,4])

plot(diff(ts(df_Data[,4])))

ARIMAfit=auto.arima(df_Data$Throughput,approximation=FALSE,trace=TRUE,seasonal=TRUE)

#Create table to show actual vs prediction  
cbinded <-cbind(as.character(df_Data$Year.and.Quarter),ARIMAfit$x,ARIMAfit$fitted,ARIMAfit$x-ARIMAfit$fitted)
View(cbinded)

Graph=forecast(ARIMAfit,4,95)
plot(Graph)


#Alternate way to plot
fcast <- forecast(ARIMAfit, h=4, level =95)
plot(fcast)

#Combined old and forecast table
newpredicted <- cbind("Q","NA",fcast$fitted,"NA")
combinedTable <- rbind(cbinded,newpredicted)
View(combinedTable)




