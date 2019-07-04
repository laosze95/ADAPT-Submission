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
    "readr",
    "ggplot",
    "car"
  )
)


library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)
library(stringr)
library(lubridate)


setwd('C:/Users/martinelli/Documents/ADAPT PROGRAM/DAY 3/workspace/')

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

#Plot an Histofram for DWELL_DAYS
#using scripts of DAY 2 or
# df_Data <- fread(paste(path,'nome file.csv'))

# Forcing numeric type of the column
df_Data$DWELL_DAYS=as.numeric(df_Data$DWELL_DAYS)
hist(df_Data$DWELL_DAYS)
boxplot(df_Data$DWELL_DAYS)
plot(df_Data$CNTR_WGT_KG,df_Data$DWELL_DAYS)

#for check information about the function make: ?name_function

#Find avg CNTR_WGT by POL using group_by
# sorted by CNTR_WGT in ascendind order
dummy <- df_Data %>% group_by(POL) %>% summarize(mn=mean(CNTR_WGT_KG)) %>% arrange(mn)%>% top_n(wt=mn, n=5)

# Create an aggregate table to compute avg. container weight for each POL and Empty/FUll containerdata
tableLongAggregate <- df_Data %>% group_by(POL, CNTR_FOE) %>% summarize(mn=mean(CNTR_WGT_KG))

#Convert the aggregate table in long format to write to wide format for ease of interpration of table
# tableWideAggregate <- df_Data %>% distinct(POL, .keep_all = TRUE) %>% group_by(POL, CNTR_FOE) %>% summarize(mn=mean(CNTR_WGT_KG))
summaryTable = dcast(tableLongAggregate, POL~CNTR_FOE, value.var="mn")

#Column subset - condition filtering
# Select all data with POL of 'IDBUN' and store into a new dataFrame df_IDBUN
df_IDBUN=filter(df_Data,POL=="IDBUN")
#or
df_IDBUN = df_Data %>% filter(POL=="IDBUN")

df_BUN=filter(df_Data, grepl(pattern="BUN", ignore.case=TRUE, x=POL))
#or
df_BUN = df_Data %>% filter(grepl(pattern="BUN", ignore.case=TRUE, x=POL))

#ROW SUBSET - sample rows
#Randomly select 5 row of data from df_Data and store it variable 'sample'
sample <- sample(1:nrow(df_Data),5)
#or
sample = df_Data %>% sample_n(5)

#How about selecting 5% of the rows from df_Data
sample = df_Data %>% sample_frac(0.05)

#Merging of data frames
# MAP POL and POD in df_Data to their country code, country name, port naem and the city name 
# by referencing to
# Portcodes Dictionary we loaded previously. 
# Store it into a new column 'POL_COUNTRY_CODE', 'POL_COUNTRY_NAME', 'POL_PORTNAME' 
# and 'POL_CITYNAME' and simiòlary for POD respectly
df_PortCodesDictionary = read_excel("Portcodes Dictionary.xlsx",sheet="Portcode Dictionary")


# # INNER JOIN 
# df_Data = merge(df_Data, df_PortCodesDictionary)
# df_Data$POL_COUNTRY_CODE = df_Data$`COUNTRY CODE`
# df_Data$POL_COUNTRY_NAME = df_Data$`COUNTRY NAME`
# df_Data$POL_PORTNAME = df_Data$`PORT NAME`
# 
# df_Data = merge(df_Data, df_PortCodesDictionary)
# df_Data$POD_COUNTRY_CODE = df_Data$`COUNTRY CODE`
# df_Data$POD_COUNTRY_NAME = df_Data$`COUNTRY NAME`
# df_Data$POD_PORTNAME = df_Data$`PORT NAME`
# 
# # OUTER JOIN 
# df_Data = merge(df_Data, df_PortCodesDictionary, by.x="POL",by.y="PORT CODE" ,all=TRUE)
# df_Data$POL_COUNTRY_CODE = df_Data$`COUNTRY CODE`
# df_Data$POL_COUNTRY_NAME = df_Data$`COUNTRY NAME`
# df_Data$POL_PORTNAME = df_Data$`PORT NAME`
# 
# df_Data = merge(df_Data, df_PortCodesDictionary, by.x="POL",by.y="PORT CODE" ,all=TRUE)
# df_Data$POD_COUNTRY_CODE = df_Data$`COUNTRY CODE`
# df_Data$POD_COUNTRY_NAME = df_Data$`COUNTRY NAME`
# df_Data$POD_PORTNAME = df_Data$`PORT NAME`

# LEFT OUTER JOIN
df_Data = merge(df_Data, df_PortCodesDictionary, by.x="POL",by.y="PORT CODE" ,all.x=TRUE)
df_Data$POL_COUNTRY_CODE = df_Data$`COUNTRY CODE`
df_Data$POL_COUNTRY_NAME = df_Data$`COUNTRY NAME`
df_Data$POL_PORTNAME = df_Data$`PORT NAME`

df_Data = merge(df_Data, df_PortCodesDictionary, by.x="POD",by.y="PORT CODE" ,all.x=TRUE)
df_Data$POD_COUNTRY_CODE = df_Data$`COUNTRY CODE`
df_Data$POD_COUNTRY_NAME = df_Data$`COUNTRY NAME`
df_Data$POD_PORTNAME = df_Data$`PORT NAME`

# RIGHT OUTER JOIN
# df_Data = merge(df_Data, df_PortCodesDictionary, by.x="POL",by.y="PORT CODE" ,all.y=TRUE)
# df_Data$POL_COUNTRY_CODE = df_Data$`COUNTRY CODE`
# df_Data$POL_COUNTRY_NAME = df_Data$`COUNTRY NAME`
# df_Data$POL_PORTNAME = df_Data$`PORT NAME`
# 
# df_Data = merge(df_Data, df_PortCodesDictionary, by.x="POD",by.y="PORT CODE" ,all.y=TRUE)
# df_Data$POD_COUNTRY_CODE = df_Data$`COUNTRY CODE`
# df_Data$POD_COUNTRY_NAME = df_Data$`COUNTRY NAME`
# df_Data$POD_PORTNAME = df_Data$`PORT NAME`
# 
# # CROSS JOIN
# df_Data = merge(df_Data, df_PortCodesDictionary, by.x='',by.y='')
# df_Data$POL_COUNTRY_CODE = df_Data$`COUNTRY CODE`
# df_Data$POL_COUNTRY_NAME = df_Data$`COUNTRY NAME`
# df_Data$POL_PORTNAME = df_Data$`PORT NAME`
# 
# df_Data = merge(df_Data, df_PortCodesDictionary, by.x='',by.y='')
# df_Data$POD_COUNTRY_CODE = df_Data$`COUNTRY CODE`
# df_Data$POD_COUNTRY_NAME = df_Data$`COUNTRY NAME`
# df_Data$POD_PORTNAME = df_Data$`PORT NAME`

# Remove duplicate record in the dataset by following fields

df_Data_temp <- unique(df_Data[c("CNTR_N", "CNTR_WGT_KG","POL","POD","LOAD_VSL_NAME","DISC_VSL_NAME")])


# or with the duplicate pattern
#df_Data[duplicated(df_Data[c("CNTR_N", "CNTR_WGT_KG","POL","POD","LOAD_VSL_NAME","DISC_VSL_NAME")]),]

#For see the frequency of the ripetition
# DEBUG: View(table(df_Data$CNTR_N))

#GGPLOT

#Create a box using ggplot to display the CNTR_WGT_KG  of all transactions by empty and full containers

df_Data$CNTR_WGT_KG= as.numeric(df_Data$CNTR_WGT_KG)
df_Data %>% ggplot(aes(CNTR_FOE,CNTR_WGT_KG)) + geom_boxplot()
# DEBUG: colnames(df_Data) 

########### Statistical analysis in R
### Understand the structure of the mtcars dataset
str(mtcars)

# How many data item are there? col: 11
ncol(mtcars)

# How many rows of data? row: 32
nrow(mtcars)

#Missing data columns
sapply(mtcars, function(x) sum(is.na(x)))

#Missing data rows
rowsum(is.na(mtcars))

# How many numerical data?
#  mpg  cyl disp   hp drat   wt qsec   vs   am gear carb
numericsData <- unlist(lapply(mtcars, is.numeric))

# How many categorical data? NOPE

#Any missing data?
summary(mtcars)

#Find the mode of Type transmission
mtcars2 <-
   within(mtcars,{
     vs <- factor(vs, labels = c("V","S"))
     am <- factor(am, labels = c("automatic","manual"))
     cyl <- ordered(cyl)
     gear <- ordered(gear)
     carb <- ordered(carb)
})
mtcars=mtcars2

attach(mtcars2)
summary(mtcars2)
IQR(mpg)
mean(mpg)
median(mpg)
quantile(mpg)
quantile(mpg, prob = c(0.15,0.25, 0.35))
  
#mode
#Create the funcion
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(am)
match(am, unique(am))

tabulate(match(am,unique(am)))

which.max(tabulate(match(am,unique(am))))
unique(am)[which.max(tabulate(match(am,unique(am))))]

#Show the breakdown of automatic and manual cars using simple plot
ggplot(mtcars,aes(x=mpg)) + geom_histogram()

ggplot(mtcars2,aes(x=mpg)) + geom_histogram()

hist(mpg)
hist(wt)
boxplot(mpg)
plot(mpg,wt)

var(mpg)
# or sd(mpg)
dev_std = sqrt(var(mpg))
# LEFT skewed

attach(mtcars2)
plot(density(mpg))

# disp: more correlation
cov(mpg,disp)

# disp: worst correlation

#What is the covariance and correlation of MPG and WT?
cov(mpg,wt)
cor(mpg,wt)

attach(mtcars2)
pairs(mtcars2)

#Pearson 's product-moment Correlation
cor.test(mpg, wt, method="pearson")

# Is the mean MPG of automatic cars different from manual cars? 
# State the null and hypothesis.
# Assume level of significance = 5%

#We need to perfom the the t statistic T student

# t.test()
# H0: the mean mpg between manual and automatic cars are equal
# H1: The mean mpg between manual and automatic cars are not equal

t.test(mpg ~ am, var.equal=FALSE)
# 95 % confidence interval
# THE p-value = 0.001374 so the p-value < 1-0.95 H0 is rejected. There is a significant that
# the mean of mpg between manual and automatic are not equal.

# THE TEST of Equal Variances.
# H0 The variances of the two samples are equal
# H1 the variances of the two sample are not equal
# p-value < 0.005 so H0 is rejected. There is evidence the the two variance are not equal
library(car)
leveneTest(mpg~am)

# The Test of normality
#H0 MPG follows a normal distribution
#H1 MPG does not follow a normal distribution
# in this case p-value = 0.1229, p-value > 0.05 H0 is not rejected.
shapiro.test(mtcars$mpg)

# WE use the plot --> The Shapiro test
# Test of normality
qqnorm(mpg)
qqline(mpg)

# Basic Regression 
# State the linear equation that describes the relationship between MPG and type of transmission
# CREATING LINEAR MODEL
linearMod <- lm(formula= mpg~am, data=mtcars)
summary(linearMod)

# Predict the weight = 3000 lb
linearMod2 <- lm(formula= mpg~wt, data=mtcars)
summary(linearMod2)
