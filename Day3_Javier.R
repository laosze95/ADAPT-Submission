#Javier Kan Day 3 Exercise
library(dplyr)
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(forecast)
library(GGally)
library(PerformanceAnalytics)
library(psych)
library(reshape2)
library(stringr)
library(tseries)
library(InformationValue)
library(data.table)

#Set working directory
setwd("C:/Users/javierk/Desktop/Admin/Main/2019/ADAPT/ADAPT Practical")
getwd()
Path = "C:/Users/javierk/Desktop/Admin/Main/2019/ADAPT/ADAPT Practical"
Path

#Assign path 
filenames = dir(Path)

#Create new dataframe
df_Data = data.frame()

#For each file check for .csv extension
for(file in filenames){
  if(grepl(".csv",file,ignore.case = TRUE))
  {
    #Read csv file
    temp=read.csv(file)
    #Append data
    df_Data = rbind(df_Data,temp)
  }
  
}

#Read xlsx file
Port_codes = data.frame()
Port_codes = read_xlsx("C:/Users/javierk/Desktop/Admin/Main/2019/ADAPT/ADAPT Practical/Portcodes Dictionary.xlsx")

#View data
View(df_Data)

dwell <- df_Data$DWELL_DAYS
#Histogram
hist(dwell, border="blue", col="orange")
#boxplot
boxplot(dwell)
#Scatter plot
plot(df_Data$CNTR_WGT_KG, df_Data$DWELL_DAYS)

#Find avg CNTR_WGT by POL using group by , arrange in ascending and display top 5
AvgWGT <- df_Data %>%
  group_by(POL) %>%
  summarise(meanwgt = mean(CNTR_WGT_KG)) %>%
  arrange(meanwgt) %>%
  top_n(wt = meanwgt, n = 5)
AvgWGT

#Find avg CNTR_WGT by POL and CNTR_FOE
AvgWGT <- df_Data %>%
  group_by(POL, CNTR_FOE) %>%
  summarise(meanwgt = mean(CNTR_WGT_KG))
AvgWGT

#Convert from long to wide format
dcast(AvgWGT, POL ~ CNTR_FOE, value.variable = meanwgt)

#Filter column by IDBUN
df_IDBUN <- df_Data %>% filter(POL=="IDBUN")
View(df_IDBUN)

#Filter column contains BUN
df_IDBUN <- df_Data %>% filter(grepl(pattern = "BUN",ignore.case = TRUE, POL))
#Filter column contains IDBUN, SGSIN or MYABC
df_IDBUN <- df_Data %>% filter(POL %in% c("IDBUN","SGSIN","MYABC"))
View(df_IDBUN)

#1 to nrow(return num of rows), select 5 rows
View(df_Data[sample(1:nrow(df_Data),5),])
#Alternate way to get 5 sampels
sample_n(df_Data,5)

#5% of the row
View(df_Data[sample(1:nrow(df_Data),0.05*nrow(df_Data)),])
#Alternate way to get 5% of the row
smple = df_Data %>% sample_frac(0.05)
View(smple)

#Merge table by POL 
Jointtable <- merge(df_Data, Port_codes, by.x="POL", by.y="PORT CODE", all.x = TRUE)

#Merge table by POD
Jointtable <-merge(Jointtable, Port_codes, by.x="POD", by.y="PORT CODE", all.x = TRUE)
View(Jointtable)

#Show duplicate records
ShowDup <- df_Data[duplicated(df_Data[c("CNTR_N","CNTR_WGT_KG","POL","POD","LOAD_VSL_NAME","DISC_VSL_NAME")]),]
View(ShowDup)

#GGplot boxplot
ggplot(df_Data) + geom_boxplot(aes(CNTR_FOE,CNTR_WGT_KG))

#GGplot histogram
ggplot(df_Data, aes(x=CNTR_WGT_KG)) + 
geom_histogram(color="darkblue", fill="lightblue") 

#Understanding the structure of mtcars
str(mtcars)
dim(mtcars)
colSums(is.na(mtcars))
summary(mtcars)

#Change to factor
mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V","S"))
  am <- factor(am, labels = c("automatic","Manual"))
  cyl <- ordered(cyl)
  gear <- ordered(gear)
  carb <- ordered(carb)
})
mtcars2

#Mode
#Create function
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
getmode(mtcars2$am)

#Variance
var(mpg)
#Standard deviation
sd(mpg)

#Density chart
attach(mtcars2)
plot(density(mpg))

#Covariance and correlation
cov(mtcars$cyl,mtcars$mpg)
cor(mtcars$cyl,mtcars$mpg)

#Test of correlation
pairs(mtcars)
cor.test(mpg,wt,method="pearson")

