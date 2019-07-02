install.packages(
  c(
    "ggplot2",
    "GGally",
    "Psych",
    "Tseries",
    "forecast",
    "InformationValue",
    "PerformanceAnalytics",
    "readxl",
    "dplyr",
    "reshape2",
    "stringr",
    "lubridate",
    "readr"
  )
)


library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)
library(stringr)
library(lubridate)


setwd('C:/Users/martinelli/Documents/ADAPT PROGRAM/DAY 2/workspace/')

# Load every csv files 

df_Data <- 'null'
temp <- 'null'
Port_codes <- 'null'

filenames <- list.files(path = "C:/Users/martinelli/Documents/ADAPT PROGRAM/DAY 2/workspace/", pattern = "*.csv", full.names = T)
for (i in 1:length(filenames)){
  temp <- assign(filenames[i], read.csv(filenames[i]))
  df_Data <- rbind(df_Data, temp) 
}
  
# READ_EXCEL read the Data Frame directly
Port_codes <- read_excel("Portcodes Dictionary.xlsx",sheet="Portcode Dictionary")

# Determinate dimension of df_Data, number of columsn and rows of df_Data
ncol(df_Data)
nrow(df_Data)
# the function dim summarizes the dimension of df_Data
dim(df_Data)

#Top few rows and the bottom few rows (in this case i extract the first and the last 20 rows)
head(df_Data, 20)
tail(df_Data, 20)

# extract random rows in head and tail
random_max_ncol_nrow <- sample(1:100, 1)
head(df_Data, random_max_ncol_nrow)
tail(df_Data, random_max_ncol_nrow)

# extract random number from nrow df_Date
sample(1:nrow(df_Data),10)
df_Data[sample(1:nrow(df_Data),10),]

# preview at some sample data (top few rows and bottom few rows)
top_few_rows <- sample(1:nrow(df_Data), 25)
bottom_few_rows <- sample(1:nrow(df_Data), 10)
#access to coordinate x, y to the data frame
df_Data[top_few_rows,]
df_Data[bottom_few_rows,]

#List out the column names in the dataset
colnames(df_Data)
colnames(Port_codes)

# Extract the name of the first 3 columns name of the dataset
head(colnames(df_Data),3)
head(colnames(Port_codes),3)

# Extract the name of the last 5 columns name of the dataset
tail(colnames(df_Data),5)
tail(colnames(Port_codes),5)

#Change columns names of the dataset to the desired ones based on the table
# LOAD_PORT_C -> POL
# DISC_PORT1_C -> POD
# LOAD_ABBR_VESSEL_LM -> LOAD_VSL_NAME
# DISC_ABBR_VESSEL_M -> DISC_VSL_NAME, ECC...

colnames(df_Data)[colnames(df_Data)=="LOAD_PORT_C"] <- "POL"
colnames(df_Data)[colnames(df_Data)=="DISC_PORT1_C"] <- "POD"
colnames(df_Data)[colnames(df_Data)=="LOAD_ABBR_VESSEL_M"] <- "LOAD_VSL_NAME"
colnames(df_Data)[colnames(df_Data)=="DISC_ABBR_VESSEL_M"] <- "DISC_VSL_NAME"
colnames(df_Data)[colnames(df_Data)=="CNTR_STATUS.C"] <- "CNTR_FOE"
colnames(df_Data)[colnames(df_Data)=="LENGTH_Q"] <- "CNTR_LENGTH"
colnames(df_Data)[colnames(df_Data)=="WT_Q"] <- "CNTR_WGT_KG"
colnames(df_Data)[colnames(df_Data)=="HEIGHT_Q"] <- "CNTR_HEIGHT"

# using rename in the best view:
df_data = df_Data %>% rename("POL"="LOAD_PORT_C")
rename(df_Data,c("POL"="LOAD_PORT_C", "POD"="DISC_PORT1_C" ))

#DISC_DT e LOAD_DT convert from scientific number into non-scientific format

format(df_Data$DISC_DT, scientific=FALSE)
format(df_Data$LOAD_DT, scientific=FALSE)

# Add new columns with specific time - datetime
# using lubridate

df_Data$dt_LOAD_DT <- as_datetime(df_Data$LOAD_DT)
df_Data$dt_DISC_DT <- as_datetime(df_Data$DISC_DT)

# Add new columns with specific time - datetime
# NO-using lubridate

df_Data$dt_LOAD_DT=as.POSIXct(strptime(df_Data$LOAD_DT,'%Y%m%d%H%M%S'))
df_Data$dt_DISC_DT=as.POSIXct(strptime(df_Data$DISC_DT,'%Y%m%d%H%M%S'))

# Obtain data type of all columns
sapply(df_Data, class)
str(df_Data)

# Create a quick summary table to undestand the data spread, it extract mode, median, ecc...
summary(df_Data)

#Find number of NAs in RF_TEMP column
sum(is.na(df_Data$RF_TEMP))

#Find number of NAs in each column in one command
colSums(is.na(df_Data))

naData = colSums(is.na(df_Data))/nrow(df_Data)
round(naData,2)

#Find avg CNTR__WGT by CNTR_FOE using group_by
df_Data %>% group_by(CNTR_FOE) %>% summarize(mean(CNTR_WGT_KG))


                     

