library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(lubridate)

setwd("C:/container")

#Combine the csv files in the folder

df_Data = data.frame()
path="C:/container"
filenames=dir(path)
for(file in filenames)
{
 if(grepl(".csv",file))
 {
   temp=read.csv(file)
   df_Data=rbind(df_Data,temp)
 }
}

#reading excel file
Port_codes = read_xlsx("C:/container/Portcodes Dictionary.xlsx")

#view content of df_Data
View(df_Data)

# check the number of rows and columns
dim(df_Data)

#view the first 10 rows of data
View(head(df_Data,10))

#random select 10 rows of data
sample(1:nrow(df_Data),10)

#list of column names
colnames(df_Data)

#select first 3 column names
colnames(df_Data) [1:3]

#change column names LOAD_PORT_C to POL and HEIGHT_Q to CNTR_HEIGHT
colnames(df_Data)[9] <- "POL"
colnames(df_Data)[27] <- "CNTR_HEIGHT"

#change scientific to number
options(scipen=999)
df_Data$DISC_DT
df_Data$LOAD_DT

#convert numeric to date time formt
as.POSIXct(strptime(df_Data$DISC_DT, format="%Y%m%d%H%M%S"))

#add new column to show the date/time of loading
df_Data$dt_LOAD_DT <- as.POSIXct(strptime(df_Data$LOAD_DT, format="%Y%m%d%H%M%S"))

#add new column to show the date/time of discharging
df_Data$dt_DISC_DT <- as.POSIXct(strptime(df_Data$DISC_DT, format="%Y%m%d%H%M%S"))

#Obtain data type
str(df_Data)
sapply(df_Data, class)

#quick summary of data
summary(df_Data)

#count the number of NA in RF_TEMP column
sum(is.na(df_Data$RF_TEMP))

#count number of NA in each column
apply(is.na(df_Data), 2, sum)
View(apply(is.na(df_Data), 2, sum))
colSums(is.na(df_Data))

#percentage of NA in each column to 2 decimals
round(colSums(is.na(df_Data))/nrow(df_Data),2)

#find avg CNTR_WGT by CNTR_FOE using group_by
df_Data %>% group_by(CNTR_FOE) %>% summarize(mean(CNTR_WGT_KG))

