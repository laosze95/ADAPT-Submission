library(lubridate)
library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(stringr)

setwd("C:/Users/btutar/Desktop/ADAPT_Training")

library(readr)
library(dplyr)

ADAPT_Training = "/csvfolder"
list.files(path="C:/Users/btutar/Desktop/ADAPT_Training", pattern="*.csv", full.names=TRUE)

path ="C:/Users/btutar/Desktop/ADAPT_Training"
filenames=dir(path)

# collecting all csv 
df1 = read.csv('C:/Users/btutar/Desktop/ADAPT_Training/ADAPT_Sample_dataset1.csv')
df2 = read.csv('C:/Users/btutar/Desktop/ADAPT_Training/ADAPT_Sample_dataset2.csv')
df3 = read.csv('C:/Users/btutar/Desktop/ADAPT_Training/ADAPT_Sample_dataset3.csv')


df_Data=data.frame()
df1


for (i in 1:length(filenames)){
  if(grepl(".csv",filenames[i]))
  {
  dummy = read.csv(filenames[i], stringsAsFactors = FALSE,check.names = FALSE)
  df_Data = rbind(df_Data,dummy)
  }
}

df4 = read_excel("C:/Users/btutar/Desktop/ADAPT_Training/Portcodes Dictionary.xlsx", sheet = "Portcode Dictionary")

# it is a piece of comments: for the dimension of dataframe
str(df_Data)
dim(df_Data)

# top few rows and bottom few rows
head(df_Data,10)
tail(df_Data,5)
# viewing the rows and columns in a smart type
View(head(df_Data,10))
View(tail(df_Data,10))

#to view of a sample dataframe 
View(sample(tail(df_Data,5)))

#spesific row
View(sample(1:nrow(df_Data),15))

df_Data[sample(1:nrow(df_Data),10),]

# to view first 3 column
View(colnames(df_Data[1, 1:3]))
colnames(df_Data)[c(1,5,8,9,11)]

df_Data = df_Data %>% rename("POL" = "LOAD_PORT_C","POD"="DISC_PORT1_C",
"LOAD_VSL_NAME"="LOAD_ABBR_VESSEL_M","DISC_VSL_NAME"="DISC_ABBR_VESSEL_M","CNTR_FOE" = "CNTR_STATUS C"
,"CNTR_LENGTH" ="LENGTH_Q","CNTR_WGT_KG" = "WT_Q","CNTR_HEIGHT"="HEIGHT_Q")

#scientific to non scientific
df_Data$dt_DISC_DT <-format(df_Data$DISC_DT,scientific = FALSE)
df_Data$dt_LOAD_DT <-format(df_Data$LOAD_DT,scientific = FALSE)
View(df_Data)

# converting number to datetime
df_Data$dt_DISC_DT  <- parse_date_time(df_Data$DISC_DT, orders = c("%Y/%m/%d %H:%M:%S"))
df_Data$dt_LOAD_DT  <- parse_date_time(df_Data$LOAD_DT, orders = c("%Y/%m/%d %H:%M:%S"))
View(df_Data)

# converting number to datetime second example not completed- df_Data$dt_LOAD_DT=as.POSIXct(strptime(df_Data$LOAD_DT)) 

#data type - summary
str(df_Data)
class(df_Data)    
View(df_Data[sample(1:nrow(df_Data),10),])
View(summary(df_Data))
summary(df_Data$CNTR_LENGTH)

#determine data availability (NAs) : number of NAs in RF_temp --> % of NAs ?

count(df_Data,RF_TEMP)

sum(is.na(df_Data$RF_TEMP))
colSums(is.na(df_Data))

NA_List <- colSums(is.na(df_Data)); NA_List[NA_List > 0]
NA_List <- colSums(is.na(df_Data)); NA_List[NA_List >= 0]

sapply(df_Data, function(x) sum(is.na(x)))
View(sapply(df_Data, function(x) sum(is.na(x))))

#determine data availability (NAs) : number of NAs in RF_temp --> 2nd version
naDATA = colSums(is.na(df_Data))/nrow(df_Data)
round(naDATA,2)

# find avg of cntr weight by cntr_FOE using group by

mean(df_Data$CNTR_WGT_KG)

df_Data %>% group_by(CNTR_FOE) %>% summarise_at(vars(CNTR_WGT_KG), funs(mean(., na.rm=TRUE)))

# find avg of cntr weight by cntr_FOE using group by --> 2nd way

df_Data %>% group_by(CNTR_FOE) %>% summarise(mean(CNTR_WGT_KG))

