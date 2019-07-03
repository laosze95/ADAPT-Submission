setwd("C:/Users/edwards/OneDrive - PSA International/ADAPT/5 days course/day3")

library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(lubridate)
path="C:/Users/edwards/OneDrive - PSA International/ADAPT/5 days course/day3"

filenames = dir(path,pattern="*.csv")

df <- read.csv(filenames[1])

str(df)

# plotting histogram
# using base R library
hist(df$DWELL_DAYS)
boxplot(df$DWELL_DAYS)
# weight vs dwell days scatter chart
# insight: dwell days is longer than 10 days for Empty containers (those with 3000kg)
plot(df$CNTR_WGT_KG, df$DWELL_DAYS)



# Find avg CNTR_WGT by POL using group_by
# Sort df by CNTR_WGT (in ascending order)
# Display only top 5 POL by CNTR_WGT
df %>% group_by(POL) %>% summarise(avgcw = mean(CNTR_WGT_KG)) %>% arrange(avgcw) %>% top_n(wt = avgcw, n=5)


# create aggregate table to compute avg. cntr weight 
# for each POL (port of loading) & FOE (Full/Empty)
polPoeWgt <- df %>% group_by(POL,CNTR_FOE) %>% summarise(avgcw = mean(CNTR_WGT_KG)) %>%  top_n(wt = avgcw, n=50)
head(polPoeWgt,20)

# convert aggregate table in long format to wide format
# where POL in row, FOE Empty is one column and FOE Full is another column
head(dcast(polPoeWgt,formula = POL ~ CNTR_FOE),15)
# using tidyr library
# install.packages("tidyr")
library(tidyr)
head(spread(polPoeWgt, CNTR_FOE,avgcw))

# filter the dataset with POL = IDBUN and store in variable df_IDBUN
df_IDBUN <- filter(df, POL == "IDBUN")
head(df_IDBUN)


# filter the dataset with POL like *BUN* and store in variable df_BUN
df_BUN <- filter(df, grepl("BUN", POL))
unique(df_BUN$POL)

# filter all ports from China 
# (POL format: country code is first 2 char, city is last 3 chars)
df_CN <- df %>% filter(grepl("^CN", POL))
unique(df_CN$POL)

# row subset - ramdomly select 5 rows 
# using base function sample()
df[sample(1:nrow(df),5),]
# using dplyr sample_n()
df %>% sample_n(5)
# row subset - ramdomly select 5% of data rows
df %>% sample_frac(.05)

# Map POL and POD to respective country code, country name, port name and city name
# by referencing to "Portcodes Dictionary" file
# Store into new columns:
#   POL_COUNTRY_CODE
#   POL_COUNTRY_NAME
#   POL_PORTNAME
#   POL_CITYNAME
#   POD_*
## list out all sheets in excel file
excel_sheets("Portcodes Dictionary.xlsx")
## load "Portcode Dictionary" sheet from excel file
Port_codes <- read_excel(path = "Portcodes Dictionary.xlsx", sheet = "Portcode Dictionary")
str(Port_codes)
dfjoin <- merge(df,Port_codes,by.x="POL",by.y="PORT CODE",all.x=T)
dfjoin <- rename(dfjoin,
       "POL_COUNTRY_CODE" = "COUNTRY CODE",
       "POL_COUNTRY_NAME" = "COUNTRY NAME",
       "POL_PORTNAME" = "PORT NAME",
       "POL_CITYNAME" = "City Name")
str(dfjoin)
dfjoin <- merge(dfjoin,Port_codes,by.x="POD",by.y="PORT CODE",all.x=T)
dfjoin <- rename(dfjoin,
                 "POD_COUNTRY_CODE" = "COUNTRY CODE",
                 "POD_COUNTRY_NAME" = "COUNTRY NAME",
                 "POD_PORTNAME" = "PORT NAME",
                 "POD_CITYNAME" = "City Name")
str(dfjoin)
dim(dfjoin)

# remove duplicate records by below fields:
#   CNTR_N
#   CNTR_WGT_KG
#   POL
#   POD
#   LOAD_VSL_NAME
#   DISC_VSL_NAME
distTx <- df[!duplicated(df[,c("CNTR_N","CNTR_WGT_KG","POL","POD","LOAD_VSL_NAME","DISC_VSL_NAME")]),]
str(distTx)
# The duplicated row which has been removed from above exercise
dupTx <- df[duplicated(df[,c("CNTR_N","CNTR_WGT_KG","POL","POD","LOAD_VSL_NAME","DISC_VSL_NAME")]),]
nrow(dupTx)
# Distinct Containers
distCntr <- df[!duplicated(df[,c("CNTR_N")]),]
NROW(distCntr)
# The two duplicate rows from first example 
dupTx <- df[duplicated(df[,c("CNTR_N","CNTR_WGT_KG","POL","POD","LOAD_VSL_NAME","DISC_VSL_NAME")]),]
filter(df, df$CNTR_N == dupTx$CNTR_N)


# GGPLOT: create boxplot to display CNTR_WGT_KG of all txn by FOE (empty/full)
df$CNTR_WGT_KG = as.numeric(df$CNTR_WGT_KG)
class(df$CNTR_WGT_KG)
df %>% ggplot(aes(CNTR_FOE,CNTR_WGT_KG)) + geom_boxplot()

# statistic
str(mtcars)

# how many data items are there?
# how many rows?
dim(mtcars)
# how many numerical data for mpg, wt, am?
# how many categorical data for mpg, wt, am?
str(mtcars)
mtc <- mtcars
# mtc$am <- as.factor(mtcars$am)
mtc$am <- factor(mtc$am,labels=c("Automatic","Manual"))
str(mtc)

# Any missing data? - NO
summary(mtc)
sapply(mtc,function(x) sum(is.na(x)))
rowSums(is.na(mtc))
# mtCars has given names for each rows
rownames(mtc)

# Quantile
quantile(mtc$mpg)
IQR(mtc$mpg)
quantile(mtc$mpg, prob = c(.15, .25, .35))

# using column names as individual variable
attach(mtc)
quantile(mpg, prob = c(.15, .25, .35))

am
match(am,unique(am))
tabulate(match(am,unique(am)))
which.max(tabulate(match(am,unique(am))))

# Exploratory Data Analysis on mtcars - simple data visualization
# show breakdown of automatic/manual using simple plot
plot(am)
# show distribution of MPG on simple plot
hist(mpg,col="blue")
# show distribution of MPG by type of transmission on simple plot
plot(am,mpg)
# show distribution of MPG by weight on simple plot
plot(wt,mpg)

# what's the variance and standard deviation of MPG?
var(mpg)
sd(mpg)

# Density distribution curve
density.default(mpg)
plot(density.default(mpg))

# which variables have positive relationship with MPG and which negative
cor(select(mtc,-c("am")))[-1,"mpg"]
# or in graph view as below
pairs(mtc)
# what is covariance & correlation of MPG and WT?
cov(mpg,wt)
cor(mpg,wt)
cor.test(mpg, wt, method = "pearson")
plot(mpg,wt)
