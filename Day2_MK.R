library("readxl")
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(lubridate)
setwd("C:/Users/kmichael/Desktop/ADAPT data")
path="C:/Users/kmichael/Desktop/ADAPT data"
filenames=dir(path)
df_Data=data.frame()

#file_list=list.files(path="C:/Users/kmichael/Desktop/ADAPT data")
for (i in 1:length(filenames))
{
if (substr(filenames[i],nchar(filenames[i])-2,nchar(filenames[i]))=="csv")
{
df=read.csv(file=paste(path,filenames[i], sep="/"),check.names = FALSE, stringsAsFactors = FALSE)
 df_Data=rbind(df,df_Data)
}
}
Port_codes = read_excel("Portcodes Dictionary.xlsx")

dim(df_Data) #to find out the number of rows and columns in a dataframe

#viewing the first or last few rows in dataset
head(df_Data, 10)
View(head(df_Data,10))
tail(df_Data, 10)
View(tail(df_Data,10))
df_Data[sample(1:nrow(df_Data), 10),] #by putting df_Data in the front, you specify u want to see the rows in that dataset.
df_Data[1:6,] #show only certain columns
df_Data[100:109, ] #shows  only certain rows. whatever is inside the [], it is columns and rows specification.

colnames(df_Data)
colnames(df_Data[(ncol(df_Data)-3):ncol(df_Data)]) #last 3 columns 

colnames(df_Data)[colnames(df_Data)=="LOAD_PORT_C"]="POL"
colnames(df_Data)[colnames(df_Data)=="DISC_PORT1_C"]="POD"
colnames(df_Data)[colnames(df_Data)=="LOAD_ABBR_VESSEL_M"]="LOAD_VSL_NAME"
colnames(df_Data)[c(12,4,6,8,27)] = c("DISC_VSL_NAME","CNTR_FOE","CNTR_LENGTH","CNTR_WGT_KG", "CNTR_HEIGHT")

df_Data$dt_DISC_DT=format(df_Data["DISC_DT"], scientific = FALSE)
df_Data$dt_LOAD_DT=format(df_Data["LOAD_DT"], scientific = FALSE)
df_Data$dt_DISC_DT = as.POSIXct(strptime(df_Data$DISC_DT, format="%Y%m%d%H%M%S"))
df_Data$dt_LOAD_DT = as.POSIXct(strptime(df_Data$LOAD_DT, format="%Y%m%d%H%M%S"))

sapply(df_Data,class) #finding out all the data class of the columns
summary(df_Data) #gives a summary of all the columns
View(summary(df_Data))
View(sapply(df_Data,class))

sum(is.na(df_Data$RF_TEMP))
sapply(df_Data, function(x) sum(is.na(x)))
colSums(is.na(df_Data))      
View(colSums(is.na(df_Data)))


df_Data %>%
  group_by(CNTR_FOE) %>%
  summarize(Mean=mean(CNTR_WGT_KG))
