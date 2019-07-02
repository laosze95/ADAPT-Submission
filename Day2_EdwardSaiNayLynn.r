
setwd("C:/Users/edwards/OneDrive - PSA International/ADAPT/5 days course/day2wd")

library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(lubridate)
path="C:/Users/edwards/OneDrive - PSA International/ADAPT/5 days course/day2wd"
filenames = dir(path,pattern="*.csv")
length(filenames)


df_Data = data.frame()
for(i in filenames){
  print(i)
  df_Data <- rbind(df_Data,read.csv(i))
  str(df_Data)
}
str(df_Data)

## list out all sheets in excel file
excel_sheets("Portcodes Dictionary.xlsx")

## load "Portcode Dictionary" sheet from excel file
Port_codes <- read_excel(path = "Portcodes Dictionary.xlsx", sheet = "Portcode Dictionary")
str(Port_codes)

## load "Relative to SG" sheet from excel file
Port_codes_SG <- read_excel(path = "Portcodes Dictionary.xlsx", sheet = "Relative to SG")
str(Port_codes_SG)

# preview data frame
View(df_Data)

## check number of rows, cols, and dimension
nrow(df_Data)
ncol(df_Data)
dim(df_Data)

## Preview at some sample data (top fiew rows and bottom few rows)
head(df_Data,10)
tail(df_Data,10)
# top 10 and bottom 10
View(rbind(head(df_Data,10),tail(df_Data,10)))
# from 10001 to 10010
View(tail(head(df_Data,10010),10))
# Sample 10 from total
View(df_Data[sample(1:nrow(df_Data),10),])
# Sample 10 from top 100
View(df_Data[sample(1:100,10),])
# Sample 10 from 10,000 to 20,000
View(df_Data[sample(10000:20000,10),])
# sample 5 from every 10 thousand rows
df2 <- data.frame()
for(i in seq(1,nrow(df_Data),by=10000)){
  print(i)
  samp <- sample(i:(i+9999),5)
  print(samp)
  df2 <- rbind(df2,df_Data[samp,])
  
}
View(df2)


# column name
colnames(df_Data)
# first 3 columns
head(colnames(df_Data),3)

findFunction("rename")

# rename columns
df_Data2 <- rename(df_Data,
    "POL" = "LOAD_PORT_C",
    "POD" = "DISC_PORT1_C",
    "LOAD_VSL_NAME" = "LOAD_ABBR_VESSEL_M",
    "DISC_VSL_NAME" = "DISC_ABBR_VESSEL_M",
    "CNTR_FOE" = "CNTR_STATUS.C",
    "CNTR_LENGTH" = "LENGTH_Q",
    "CNTR_WGT_KG" = "WT_Q",
    "CNTR_HEIGHT" = "HEIGHT_Q")


## convert DISC_DT and LOAD_DT numeric from scienctific number to char, 
# and convert to date 
# and add new columns dt_DISC_DT and dt_LOAD_DT respectively
df_Data$dt_DISC_DT <- df_Data$DISC_DT %>% as.character() %>% as.POSIXct(format="%Y%m%d%H%M%S")
df_Data$dt_LOAD_DT <- df_Data$LOAD_DT %>% as.character() %>% as.POSIXct(format="%Y%m%d%H%M%S")
head(df_Data$dt_DISC_DT)
head(df_Data$dt_LOAD_DT)
dim(df_Data)

# Data type of all columns and quick summary of table to understand data spread
str(df_Data)
summary(df_Data)

# determine numbers of NAs
# number of NAs in specific column RF_TEMP
length(which(is.na(df_Data$RF_TEMP)))
# number of NAs in each column
colSums(is.na(df_Data))
# percentage of NAs in each column
round(colSums(is.na(df_Data)) / nrow(df_Data),2)*100
# show only those columns with NA values count percentage > 0
NAcount <- round(colSums(is.na(df_Data)) / nrow(df_Data),2)*100
NAcount[NAcount>0]

# find avg CNTR_WGT by CNTR_FOE using "group_by"
df_Data2 %>% group_by(CNTR_FOE) %>% summarise(mn = mean(CNTR_WGT_KG), n=length(CNTR_WGT_KG))


