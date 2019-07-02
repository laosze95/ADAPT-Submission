library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(lubridate)

setwd("C:/Users/jingmin/Desktop/ADAPT-master/Adapt 1 to 3")
getwd()

##how to load raw data from 3 files into a single dataframe(df_Data)
myfiles = list.files(path="C:/Users/jingmin/Desktop/ADAPT-master/Adapt 1 to 3", pattern="*.csv", full.names=TRUE)
myfiles
library(readr)
library(plyr)
df_Data = ldply(myfiles, read_csv)
View(df_Data)

##how to load reference file into another dataframe(Port_codes)
Port_codes <- read_excel("Portcodes Dictionary.xlsx")
View(Port_codes)

##Determine dimension of df_Data ie.: number of columns and rows of df_Data 
dim(df_Data)
##[1] 49990    27
##row is 49990 and columns is 27

##Preview at some sample data (top few rows and bottom few rows)
View(head(df_Data))
View(tail(df_Data))
##use View in addition to see it in table format 
View(head(df_Data,10))
##to see first 10 rows 
View(sample_n(df_Data,10))
##to see sample 10 rows 
sample(1:nrow(df_Data),10)
##to see sample data first column, total 10 rows 
df_Data[sample(1:nrow(df_Data),10),]
##to see sample data FRAME entire frame, total 10 rows 

##list out column names in dataset 
colnames(df_Data)
colnames(df_Data[1:3]) 
###to see only 1st 3 column names)
ncol(df_Data)
##to see number of columns inside df_Data
colnames(df_Data)[(ncol(df_Data)-3) :ncol(df_Data)]
##to see last 4 rows of inside df_Data 
##square bracket refers to the columns/ rows to view 

##Change column names of the dataset to the desired ones based on the table below
colnames(df_Data)
colnames(df_Data)=="LENGTH_Q"
colnames(df_Data)[colnames(df_Data)=="LENGTH_Q"]
colnames(df_Data)[colnames(df_Data)=="LENGTH_Q"]="CNTR_LENGTH"
colnames(df_Data)
colnames(df_Data)=="DISC_PORT1_C"
colnames(df_Data)[colnames(df_Data)=="DISC_PORT1_C"]
colnames(df_Data)[colnames(df_Data)=="DISC_PORT1_C"]="POD"
colnames(df_Data)
colnames(df_Data)=="LOAD_ABBR_VESSEL_M"
colnames(df_Data)[colnames(df_Data)=="LOAD_ABBR_VESSEL_M"]
colnames(df_Data)[colnames(df_Data)=="LOAD_ABBR_VESSEL_M"]="LOAD_VSL_NAME"
colnames(df_Data)
colnames(df_Data)=="DISC_ABBR_VESSEL_M"
colnames(df_Data)[colnames(df_Data)=="DISC_ABBR_VESSEL_M"]
colnames(df_Data)[colnames(df_Data)=="DISC_ABBR_VESSEL_M"]="DISC_VSL_NAME"
colnames(df_Data)
colnames(df_Data)=="CNTR_STATUS C"
colnames(df_Data)[colnames(df_Data)=="CNTR_STATUS C"]
colnames(df_Data)[colnames(df_Data)=="CNTR_STATUS C"]="CNTR_FOE"
colnames(df_Data)
colnames(df_Data)=="WT_Q"
colnames(df_Data)[colnames(df_Data)=="WT_Q"]
colnames(df_Data)[colnames(df_Data)=="WT_Q"]="CNTR_WGT_KG"
colnames(df_Data)
colnames(df_Data)=="HEIGHT_Q"
colnames(df_Data)[colnames(df_Data)=="HEIGHT_Q"]
colnames(df_Data)[colnames(df_Data)=="HEIGHT_Q"]="CNTR_HEIGHT"
colnames(df_Data)
names(df_Data)

##DISC_DT and LOAD_DT are in scientific number format. we need to convert them to non-scientific format. e.g: convert 2.01807 to 20180710204408
##convert the value in the previous step to proper data formats eg: 2018-07-10 20:44:08. create 2 new columns (dt_LOAD_DT, dt_DISC_DT) to store the converted date

df_Data$DISC_DT = format(df_Data$DISC_DT, scientific = FALSE)
##scientific = true means 2.01xv with decimal, false = no decimal
df_Data$dt_DISC_DT = as.POSIXct(strptime(df_Data$DISC_DT, format = "%Y%m%d%H%M%S"))

df_Data$LOAD_DT = format(df_Data$LOAD_DT, scientific = FALSE)
##scientific = true means 2.01xv with decimal, false = no decimal
df_Data$dt_LOAD_DT = as.POSIXct(strptime(df_Data$LOAD_DT, format = "%Y%m%d%H%M%S"))

##obtain data type of all columns
##create a quick summary table to understand the data spread (did you observe any information that is critical for data preparation apart from basic summary statistics?)

sapply(df_Data, class)
str(df_Data)
summary(df_Data)
View(summary(df_Data))

##determine data availability (number of NAs) of each column
##find number of NAs in RF_TEMP column
##find number of NAs in each column in one command 

sum(is.na(df_Data$RF_TEMP))

colSums(is.na(df_Data))
#to define naSum as a variable
naSum=colSums(is.na(df_Data))
#to check false true, if NA it will be true
is.na(df_Data)
#if want to reverse to find non NA instead 
colSums(!is.na(df_Data))

##to find % of NA in dataset and rounding up 
naData = colSums(is.na(df_Data))/nrow(df_Data)
round(naData,2)

##find avg CNTR_WGT by CNTR_FOE using group_by
df_Data %>% group_by (CNTR_FOE) %>% summarize (mean(CNTR_WGT_KG))
##find more items to group by
df_Data %>% select(CNTR_FOE,DWELL_DAYS, CNTR_WGT_KG) %>% group_by(CNTR_FOE) %>% summarise_all(list(min = min, mn = mean,md = median)) %>% View()
