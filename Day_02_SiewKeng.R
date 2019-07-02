library("dplyr")
library("readxl")
library("ggplot2")
library("reshape2")
library("stringr")
library("lubridate")

setwd("D:/ADAPT")

df_Data = data.frame()
Port_code = ''

path="D:/ADAPT"
#load ADAPT_Sample_dataset*.csv into df_Data 
myfiles = dir(path)

for (file in myfiles)
{
  if(grepl("csv",file))
  {
    indata<- read.csv(file,check.names = FALSE, stringsAsFactors = FALSE)
    df_Data<-rbind(df_Data,indata)
  }

}


#load Portcodes Dictionary.xlsx into Port_code 
Port_code<-read_excel("Portcodes Dictionary.xlsx")


#get no of col and row of df_Data
dim(df_Data)

#viewing df_Data
head(df_Data, 20) 
tail(df_Data, 20)
View(head(df_Data, 20))
View(df_Data[sample(1:nrow(df_Data),20,replace = TRUE),2])
colnames(df_Data(1,1:3))
View(colnames(df_Data[1,1:3]))


#rename col
names(df_Data)[names(df_Data)== "LOAD_PORT_C"]="POL"
names(df_Data)[names(df_Data)== "DEST_PORT_C"]="POD"
names(df_Data)[names(df_Data)== "LOAD_ABBR_VESSEL_M"]="LOAD_VSL_NAME"
names(df_Data)[names(df_Data)== "DISC_ABBR_VESSEL_M"]="DISC_VSL_NAME"
names(df_Data)[names(df_Data)== "CNTR_STATUS C"]="CNTR_FOE"
names(df_Data)[names(df_Data)== "LENGTH_Q"]="CNTR_LENGTH"
names(df_Data)[names(df_Data)== "WT_Q"]="CNTR_WGT_KG"
names(df_Data)[names(df_Data)== "HEIGHT_Q"]="CNTR_HEIGHT"

#add new col from convert string into datetime
df_Data$dt_LOAD_DT <- parse_date_time(df_Data$LOAD_DT, orders = c("%Y%m%d% %H:%M:%S"))
df_Data$dt_DISC_DT <- parse_date_time(df_Data$DISC_DT, orders = c("%Y%m%d% %H:%M:%S"))


#desc df_Data
str(df_Data)
sapply(df_Data, class)
summary(df_Data)


#NA value
sum(is.na(df_Data$RF_TEMP))
colSums(is.na(df_Data))
NA_List <- colSums(is.na(df_Data)); NA_List[NA_List > 0] 
NA_List_Percent <- colSums(is.na(df_Data))/nrow(df_Data); NA_List_Percent [NA_List_Percent  > 0]


df_Data %>% group_by(CNTR_FOE) %>% summarise(mean = mean(as.numeric(CNTR_WGT_KG)))

