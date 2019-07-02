library(readxl)
library(reshape2)
library(stringr)
library(lubridate)
library(dplyr)
library(ggplot2)
setwd("C:/Users/edursun/Desktop/ADAPT/ADAPT_2")
ADAPT <- read.csv(file="C:/Users/edursun/Desktop/ADAPT/ADAPT_2/ADAPT_Sample_dataset1.csv",
                  header=TRUE, sep=",")
ADAPT <-"C:/Users/edursun/Desktop/ADAPT/ADAPT_2"
file_list <- list.files(path="C:/Users/edursun/Desktop/ADAPT/ADAPT_2", pattern="*.csv")

dummy = ""
df_Data=""

for (i in 1:length(file_list)){
  
  dummy = read.csv(paste(ADAPT,file_list[i], sep='/'), check.names=FALSE, stringsAsFactors = FALSE)
  df_Data = rbind(df_Data,dummy)
         
}

df_Data_2=df_Data[-1,]

#paste birleþtir gibi bir görev görüyor
#rbind ile de verilerin olduðu dosyadaki herþeyi birleþtiriyorum


## exceli çalýþtýrmak için
ADAPT2 <- read_excel("C:/Users/edursun/Desktop/ADAPT/ADAPT_2/Portcodes Dictionary.xlsx", sheet= "Portcode Dictionary")

dim(df_Data_2)
# satýr ve sütnlar için yukarýdakini kullan
head(df_Data_2)
tail(df_Data_2)
#verinin baþý ve sonu için kullanýyoruz

t=sample(1:nrow(df_Data_2),10)
df_Data_2[t,5]

z=colnames(df_Data_2)
z[1:3]

#rename the column names
library(dplyr)

df_Data_2 %>% 
  rename (
    "POL"="LOAD_PORT_C",
   "POD"="DISC_PORT1_C",
  "LOAD_VSL_NAME"="LOAD_ABBR_VESSEL_M",
  "DISC_VSL_NAME"= "DISC_ABBR_VESSEL_M" ,
  "CNTR_FOE"="CNTR_STATUS C" ,
  "CNTR_LENGTH"="LENGTH_Q" ,
  "CNTR_WGT_KG"= "WT_Q",
  "CNTR_HEIGHT"="HEIGHT_Q"
       )

# New column with $ & date format with POSIxct, strptime
df_Data_2=df_Data[-1,]
df_Data_2$dt_DISC_DT = as.POSIXct(strptime(df_Data_2$DISC_DT, format = "%Y%m%d%H%M%S")) #20180808042530
df_Data_2$dt_LOAD_DT = as.POSIXct(strptime(df_Data_2$LOAD_DT, format = "%Y%m%d%H%M%S")) #20180808042530


#class & summary of the data frame
Class(df_Data_2)
class(df_Data_2)
sapply(df_Data_2, class)
summary(df_Data_2)


#number of NAs

sapply(df_Data_2$RF_TEMP, function(x) sum(length(which(is.na(x))))) 
sum(is.na(df_Data_2$RF_TEMP)) #only for the specific column

sapply(df_Data_2, function(x) sum(is.na(x))) #all columns

nadata = sapply(df_Data_2, function(x) sum(is.na(x)))/nrow(df_Data_2) # % of NA
round(nadata,2) #yuvarlamak

library(dplyr)


df_Data_2$WT_Q <- as.numeric(as.character(df_Data_2$WT_Q))  # karakteri numeric çevir.

df_Data_2 %>%
  group_by(CNTR_FOE) %>%
  summarise(mean(WT_Q))

