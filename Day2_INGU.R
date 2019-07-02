library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(lubridate)

setwd("./sample(0702)")
getwd()

## 
df_Data1 <- read.csv("C:/Users/user/Documents/sample(0702)/ADAPT_Sample_dataset1.csv")
df_Data2 <- read.csv("C:/Users/user/Documents/sample(0702)/ADAPT_Sample_dataset2.csv")
df_Data3 <- read.csv("C:/Users/user/Documents/sample(0702)/ADAPT_Sample_dataset3.csv")
df_Data <- rbind(df_Data1,df_Data2,df_Data3)

str(df_Data)

## Loop - to read severl CSV files without not CSV files by using grepl ##
df_dir <-("C:/Users/user/Documents/sample(0702)")
file_list <- list.files(df_dir)
data <- data.frame()
for(file in file_list) {
  print(file)
  if(grepl(".csv",file,ignore.case = TRUE))
  {
    temp <- read.csv(paste(df_dir,file,sep ="\\"), header = TRUE, sep=",", stringsAsFactors = FALSE)
    data <- rbind(data,temp)
  }
}

## Loop - to read severl CSV files without not CSV files by using grepl by Xuzhi ##
path <-("C:/Users/user/Documents/sample(0702)")
file_names <- dir(path)
df_Data <- data.frame()

for(i in 1:length(filenames)){
  if(grepl(x = file_names[i],pattern=".csv",ignore.case = TRUE))
  {
    IndData <- read.csv(file=file_names[i], check.names = FALSE,stringsAsFactors = FALSE)
    df_Data <- rbind(IndData,df_Data)
  }
}

rm(data)
rm(df_data)
rm(df_Data1)
rm(df_Data2)
rm(df_Data3)
rm(df_dir_data)
rm(temp)
rm(df_dir)
rm(dir)
rm(file)
rm(file_list)


## Excel File Upload for just one File
Port_codes <- read_xlsx("C:/Users/user/Documents/sample(0702)/Portcodes Dictionary.xlsx")

## dimension for the Data Row & Column
dim(df_Data)

## Preview at some sample data(Top few rows) on Dateset
View(head(df_Data))
View(head(df_Data))
## PReview at some sample data(bottom few rows) on Dateset
View(tail(df_Data))

## Randomly preview of some data column
## first find sample rows and then print data columns based on rows.
sample(1:nrow(df_Data),10)
df_Data[sample(1:nrow(df_Data),10),]

## List out the column names in the dataset 
names(df_Data)[1:3]
names(df_Data)[25:27]

test3 <- rename(test3, "idxx" = "id")


names(df_Data)[names(df_Data) == "LOAD_PORT_C"] <- "POL"
names(df_Data)[names(df_Data) == "DISC_PORT1_C"] <- "POD"
names(df_Data)[names(df_Data) == "LOAD_ABBR_VESSEL_M"] <- "LOAD_VSL_NAME"
names(df_Data)[names(df_Data) == "DISC_ABBR_VESSEL_M"] <- "DISC_VSL_NAME"
names(df_Data)[names(df_Data) == "CNTR_STATUS.C"] <- "CNTR_FOE"
names(df_Data)[names(df_Data) == "LENGTH_Q"] <- "CNTR_LENGTH"
names(df_Data)[names(df_Data) == "WT_Q"] <- "CNTR_WGT_KG"
names(df_Data)[names(df_Data) == "HEIGHT_Q"] <- "CNTR_HEIGHT"

## If knowing the column seq, do as below
tempData <- df_Data
names(tempData)[c(1,2)]=c("test1","test2")

#Rename columns
tempData %>% rename("POL"="LOAD_PORT_C","POD"="DIS_PORT1_c","LOAD_VSL_NAME"="LOAD_ABBR_VESSEL_M","DISC_VSL_NAME"="DISC_ABBR_VESSEL_M","CNTR_FOE"="CNTR_STATUS_C","CNTR_LENGTH"="LENGTH_Q","CNTR_WGT_KG"="WT_Q","CNTR_HEIGHT"="HEIGHT_Q")


## e number to numbers and change Date format to new varience
options("scipen" = 100)
df_Data$DISC_DT
df_Data$DISC_DT <- format(df_Data$DISC_DT, scientific = FALSE)
df_Data$dt_DISC_DT=as.POSIXlt(strptime(df_Data$DISC_DT, format = '%Y%m%d%H%M%S')) 

options("scipen" = 100)
df_Data$LOAD_DT
df_Data$LOAD_DT <- format(df_Data$LOAD_DT, scientific = FALSE)
df_Data$dt_LOAD_DT=as.POSIXlt(strptime(df_Data$LOAD_DT, format = '%Y%m%d%H%M%S'))

## display all column data type 
sapply(df_Data, class)
## Quick summary Data Spread
summary(df_Data)

## nubmer of NAs in RF_TEMP Column
sum(is.na(df_Data$RF_TEMP))
## number of NAs in each column in one command -2 command are same.
sapply(df_Data, function(y) sum(is.na(y)))  
colSums(is.na(df_Data))

## pecentage of NAs in each column
round(colSums(is.na(df_Data))/nrow(df_Data),2)

## avg CNTR_WGT by CNTR_FOE using groyup_by
df_Data %>% group_by(CNTR_FOE) %>% summarise(mean(CNTR_WGT_KG))
