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

##DAY 3 - to create a hist for DWELL_DAYS 
hist(df_Data$DWELL_DAYS)

#to create boxplot for DWELL_DAYS
boxplot(df_Data$DWELL_DAYS)

#to create plot for CNTR_WGT_KG and DWELL_DAY
plot(df_Data$CNTR_WGT_KG, df_Data$DWELL_DAYS)

#type ?hist to know how to change colors 

#find average CNTR_WGT by POL using group_by
#Sort df_Data by CNTR_WGT (in ascending order)
#display only top 5 POL by CNTR_WGT

library(dplyr)

library(data.table)
df_Data=fread("ADAPT_data_for_day3.csv")
aggTable = df_Data %>%
    group_by(POL) %>%
    summarize(mn = mean(CNTR_WGT_KG)) %>%
    arrange (mn) %>%
    top_n(5,mn)
View(aggTable)

##convert the aggregate table in long format to wide format for ease of intepretation of table 

aggTable = df_Data %>%
    group_by(POL, CNTR_FOE) %>%
    summarize(mn = mean(CNTR_WGT_KG))
SummaryTable = dcast(aggTable, POL ~ CNTR_FOE, value.var="mn")

##Column subset - conditional filtering: select all data with POL of 'IDBUN' and store it into a variable df_IDBUN

df_IDBUN =  data.frame(df_Data %>% filter(POL == "IDBUN"))
View(df_IDBUN)
##remember == must use 2 equal symbol

##Column subset - wildcard filtering: select all data with POL containing "BUN" and store it into a variable df_BUN
df_BUN = data.frame(df_Data %>% filter(grepl(pattern="BUN",ignore.case=TRUE, x=POL)))
View(df_BUN)

##Row subset - sample rows
#randomly select 5 rows of data from df_Data and store it variable 'sample'
#how about selecting 5% of the rows from df_Data?

#method 1:
sample = df_Data[sample(nrow(df_Data), 5), ]
View(sample)                                       
sample = df_Data[sample(nrow(df_Data), 0.05*nrow(df_Data)), ]
View(sample) 

#method 2:
sample = df_Data %>% sample_n(5)
sample= df_Data %>% sample_frac(0.05)

##if you want to find sample 5 rows with column 3 & 5
sample = df_Data[sample(nrow(df_Data), 5),c(3,5)]
View(sample)    

##merging of data frames
#map POL and POD in df_Data to their country code, country name, port name and city name by referencing to 'Portcodes Dictornary' we loaded previously. Store it into new column, 'POL_COUNTRY_CODE', 'POL_COUNTRY_NAME', 'POL_PORTNAME' and 'POL_CITYNAME' and similarly for POD respectively

library(readxl)
Port_codes <- read_excel("Portcodes Dictionary.xlsx")
View(Port_codes)

#do a left outer join, to include all the rows of your data frame x and only those from y that match, specify all.x=TRUE

df_Data = merge(x= df_Data, y=Port_codes, by.x="POL", by.y="PORT CODE", all.x=TRUE)
colnames(df_Data)
colnames(df_Data)=="COUNTRY CODE.x"
colnames(df_Data)[colnames(df_Data)=="COUNTRY CODE.x"]
colnames(df_Data)[colnames(df_Data)=="COUNTRY CODE.x"]="POL_COUNTRY_CODE"
colnames(df_Data)
colnames(df_Data)=="COUNTRY NAME.x"
colnames(df_Data)[colnames(df_Data)=="COUNTRY NAME.x"]
colnames(df_Data)[colnames(df_Data)=="COUNTRY NAME.x"]="POL_COUNTRY_NAME"
colnames(df_Data)
colnames(df_Data)=="PORT NAME.x"
colnames(df_Data)[colnames(df_Data)=="PORT NAME.x"]
colnames(df_Data)[colnames(df_Data)=="PORT NAME.x"]="POL_PORT_NAME"
colnames(df_Data)
colnames(df_Data)=="City Name.x"
colnames(df_Data)[colnames(df_Data)=="City Name.x"]
colnames(df_Data)[colnames(df_Data)=="City Name.x"]="POL_CITY_NAME"
colnames(df_Data)

df_Data = merge(x= df_Data, y=Port_codes, by.x="POD", by.y="PORT CODE", all.x=TRUE)
colnames(df_Data)
colnames(df_Data)=="COUNTRY CODE"
colnames(df_Data)[colnames(df_Data)=="COUNTRY CODE"]
colnames(df_Data)[colnames(df_Data)=="COUNTRY CODE"]="PODL_COUNTRY_CODE"
colnames(df_Data)
colnames(df_Data)=="COUNTRY NAME"
colnames(df_Data)[colnames(df_Data)=="COUNTRY NAME"]
colnames(df_Data)[colnames(df_Data)=="COUNTRY NAME"]="POD_COUNTRY_NAME"
colnames(df_Data)
colnames(df_Data)=="PORT NAME"
colnames(df_Data)[colnames(df_Data)=="PORT NAME"]
colnames(df_Data)[colnames(df_Data)=="PORT NAME"]="POD_PORT_NAME"
colnames(df_Data)
colnames(df_Data)=="City Name"
colnames(df_Data)[colnames(df_Data)=="City Name"]
colnames(df_Data)[colnames(df_Data)=="City Name"]="POD_CITY_NAME"
colnames(df_Data)
colnames(df_Data)=="PODL_COUNTRY_CODE"
colnames(df_Data)[colnames(df_Data)=="PODL_COUNTRY_CODE"]
colnames(df_Data)[colnames(df_Data)=="PODL_COUNTRY_CODE"]="POD_COUNTRY_CODE"
colnames(df_Data)

##remove duplicate records in the dataset by following fields: 
#CNTR_N
#CNTR_WGT_KG
#POL
#POD
#LOAD_VSL_NAME
#DISC_VSL_NAME

df_Data=df_Data %>% distinct(CNTR_N, CNTR_WGT_KG, POL, POD, LOAD_VSL_NAME, DISC_VSL_NAME, .keep_all=TRUE)
View(df_Data)

##create a box plot using ggplot to display the CNTR_WGT_KG of all transactions by empty and full containers

p <- ggplot(df_Data, aes(x=df_Data$CNTR_FOE, y=df_Data$CNTR_WGT_KG)) + 
    geom_boxplot()

##create a histogram using ggplot to display the CNTR_WGT_KG of all transactions by empty and full containers

p <- ggplot(df_Data, aes(color = CNTR_FOE, x=CNTR_WGT_KG)) + 
    geom_histogram()

p + scale_color_brewer(palette="Accent") + 
    theme_minimal()+theme(legend.position="top")

#understanding structure of mtcars dataset
#how many data items are there
#how many rows of data?
#how many numerical data?
#how many categorical data?
#any missing data? 
#min max mean of MPG
#mode of type of transmission

#to find data items and rows 
str(mtcars)

#to find min max etc
summary(mtcars)

#to find number of rows and columns
dim(mtcars)

#to find any missing values  
#row-wise missing data
rowSums(is.na(mtcars))

#to find mode of type of transmission 
#create the function
attach(mtcars)
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv [which.max(tabulate(match(v,uniqv)))]
}
#match(am,unique(am))
#tabulate(match(am,unique(am)))
#which.max(tabulate(match(am,unique(am))))

#to find IQR of MPG
IQR(mtcars$mpg)

#to convert AM to factor for auto or manual
mtcars$am <- factor(mtcars$am, labels = c("automatic","manual"))

#show the breakdown of auto and manual cars using a simple plot
plot(mtcars$am)

#show the distribution of MPG on a simple plot
hist(mtcars$mpg)

#show the distribution of MPG by type of transmission on a simple plot
p <- ggplot(mtcars, aes(x=mtcars$mpg, y=mtcars$am)) + 
    geom_boxplot()

#show the distribution of MPG and weight on a simple plot 
#show scatterplot 

#what is the variance and standard deviation of MPG? 
var(mtcars$mpg)
sd(mtcars$mpg)

#which variables have a positive relationship with MPG?
#which variables have a negative relationship with MPG? use simple plots

pairs(mtcars[,1:4])
#to see first 4 variables first
pairs(mtcars)
#to compare all variables
#mpg and disp - negative
#mpg and hp - negative
#mpg and drat - positive
#mpg and wt - negative
#mpg and qsec- positive

#what is the covariance and correlation of MPG and WT? 
cov(mtcars$mpg,mtcars$wt)
cor(mtcars$mpg,mtcars$wt)
