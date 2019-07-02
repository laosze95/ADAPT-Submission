library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(lubridate)

setwd("C:/Users/maloise/Desktop/ADAPT/Day 2/ADAPT data")
path="C:/Users/maloise/Desktop/ADAPT/Day 2/ADAPT data"

filenames=dir(path)
df_Data= ""

for(i in 1:length(filenames)){
  if(grepl(x = filenames[i],pattern = "csv")){
    
       IndData=read.csv(paste(path,filenames[i], sep = "/"), check.names = FALSE, stringsAsFactors = 
                        FALSE)
       df_Data= rbind(IndData,df_Data)
  }
}


Port_codes=read_excel("Portcodes Dictionary.xlsx")

names(df_Data)[names(df_Data) == 'LOAD_PORT_C']='POL'
names(df_Data)[names(df_Data) == 'DISC_PORT1_C']='POD'
names(df_Data)[names(df_Data) == 'LOAD_ABBR_VESSEL_M']='LOAD_VSL_NAME'
names(df_Data)[names(df_Data) == 'DISC_ABBR_VESSEL_M']='DISC_VSL_NAME'
names(df_Data)[names(df_Data) == 'CNTR_STATUS C']='CNTR_FOE'
names(df_Data)[names(df_Data) == 'LENGHT_Q']='CNTR_LENGTH'
names(df_Data)[names(df_Data) == 'WT_Q']='CNTR_WGT_KG'
names(df_Data)[names(df_Data) == 'HEIGHT_Q']='CNTR_HEIGHT'

colnames(df_Data)[1:3]
colnames(df_Data)
View(df_Data)
head(df_Data)
df_Data$dt_LOAD_DT=as.POSIXct(strptime(df_Data$LOAD_DT, "%Y%m%d%H%M%S"))
df_Data$dt_DISC_DT=as.POSIXct(strptime(df_Data$DISC_DT, "%Y%m%d%H%M%S"))
is.na(df_Data)
colnames(df_Data)
mean(CNTR_WGT_KG)
df_Data %>% group_by(CNTR_FOE) %>% summarize(mean(CNTR_WGT_KG))

