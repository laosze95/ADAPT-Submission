#Javier Kan Day 2 Exercise
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

#Read xlsx file
Port_codes = data.frame()
Port_codes = read_xlsx("C:/Users/javierk/Desktop/Admin/Main/2019/ADAPT/ADAPT Practical/Portcodes Dictionary.xlsx")

#View data
View(df_Data)

#Count num of cols and rows
dim(df_Data)

#Check top and bottom few rows
head(df_Data)
tail(df_Data)
View(head(df_Data))

sample(1:nrow(df_Data),10)
#1 to nrow(return num of rows), select 10 rows
View(df_Data[sample(1:nrow(df_Data),10),])

#Change to list and find column name
colnames(df_Data)
#Find first 3 column, brackets declare outside the function
colnames(df_Data)[1:3]

#Change col name, old name = new name
colnames(df_Data)[colnames(df_Data)=="LOAD_PORT_C"] = "POL"
colnames(df_Data)[colnames(df_Data)=="DISC_PORT1_C"] = "POD"
colnames(df_Data)[colnames(df_Data)=="LOAD_ABBR_VESSEL_M"] = "LOAD_VSL_NAME"
colnames(df_Data)[colnames(df_Data)=="DISC_ABBR_VESSEL_M"] = "DISC_VSL_NAME"
colnames(df_Data)[colnames(df_Data)=="CNTR_STATUS.C"] = "CNTR_FOE"
colnames(df_Data)[colnames(df_Data)=="LENGTH_Q"] = "CNTR_LENGTH"
colnames(df_Data)[colnames(df_Data)=="WT_Q"] = "CNTR_WGT_KG"
colnames(df_Data)[colnames(df_Data)=="HEIGHT_Q"] = "CNTR_HEIGHT"

#Using dplyr function, new name = old name
#df_Data = df_Data %>% rename("POL"="LOAD_PORT_C","POD"="DISC_PORT1_C")

#Convert format to non scientific
df_Data$LOAD_DT = format(df_Data$LOAD_DT, scientific = FALSE)
df_Data$DISC_DT = format(df_Data$DISC_DT, scientific = FALSE)

#Convert num to time and create 2 new columns
df_Data$dt_LOAD_DT = as.POSIXct(df_Data$LOAD_DT, format="%Y%m%d%H%M%S")
df_Data$dt_DISC_DT = as.POSIXct(df_Data$DISC_DT, format="%Y%m%d%H%M%S")

#Obtain data type of each column
sapply(df_Data,class)
#Alternate way of obtaining data type
str(df_Data)

#Get summary
View(summary(df_Data))

#Find num of NA in RF_TEMP
sum(is.na(df_Data$RF_TEMP))
#Find NA in each column
colSums(is.na(df_Data))

#Check % of NA for each column
naData = colSums(is.na(df_Data))/nrow(df_Data)
round(naData,2)

#Find avg CNTR_WGT by CNTR_FOE using group by
AvgWGT <- df_Data %>%
  group_by(CNTR_FOE) %>%
  summarise(mean(CNTR_WGT_KG))
AvgWGT


