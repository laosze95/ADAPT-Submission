
##path = "C:/Users/user/Documents/Day2/ADAPT_data_for_day3.csv"
## df_data = fread(paste(path,"ADAPT_data_for_day3.csv"))

## Data Loading
df_Data <- read.csv("C:/Users/user/Documents/Day2/ADAPT_data_for_day3.csv")
df_Data$DWELL_DAYS

##Plot a histogram for DWELL_DAYS
hist(df_Data$DWELL_DAYS)
boxplot(df_Data$DWELL_DAYS)
plot(df_Data$CNTR_WGT_KG, df_Data$DWELL_DAYS)

##Find avg CNTR_WGT by POIL usning group_by
df_Data %>% 
  group_by(POL) %>% 
  summarise(mean(CNTR_WGT_KG))

##sort df_Data by CNTR_WGT(in ascending order)
df_Data %>% 
  group_by(POL) %>% 
  summarise(mn = mean(CNTR_WGT_KG)) %>% 
   arrange(mn)

## Display only top 5 POL by CNTR_WGT
aggTable = df_Data %>% 
  group_by(POL) %>% 
  summarise(mn = mean(CNTR_WGT_KG)) %>% 
   arrange(mn) %>% 
    top_n(wt=mn,n=5)

View(aggTable)

## Create an aggregate table to compute avg.container weight for ach POL and Empty/Full container
aggTable = df_Data %>% 
  group_by(POL,CNTR_FOE) %>%
  summarise(mn = mean(CNTR_WGT_KG)) %>% 
  arrange(mn) %>% 
  top_n(wt=mn,n=5)
View(aggTable)

## Convert the aggregate table in long format to wide format for ease of interpretation of table.
library(reshape2)
dcast(aggTable, POL ~ CNTR_FOE, var.value = mn)
dcast(aggTable, POL ~ CNTR_FOE, na.rm=TRUE)

## Column subset - conditional filtering
 ## Select all data with POL of 'IDBUN' and store it into a variable df_IDBUN
df_IDBUN <- subset(df_Data, POL== "IDBUN")
df_IDBUN <- df_Data %>% filter(POL == "IDBUN")
View(df_IDBUN)  
  
## Column subset - wildcard filtering
 ## Select ll data with POL containing 'BUN' and store in into a variable df_BUN
df_BUN <- df_Data %>% filter(grepl("BUN",POL))
df_BUN <- df_Data %>% filter(grepl(pattern ="BUN", ignore.case = TRUE, x = POL))
## filtering for several values
df_BUN <- df_Data %>% filter(POL %in% c("IDBUN","SGSIN"))


#Row subset - sample rows
 ## Ramdomly select 5 row of data from df_Data store it variable "sample"
  sample = df_Data %>% sample_n(5)
 ## How about selecting 5% of the rows from df_Dat? 
  sample = df_Data %>% sample_frac(0.05)

## Map POL and POD in df_Data to their country code, country name, port name and city name by referencing to "Pordcodes"
## "POL"COUNTRUY_NAME", "PORT NAME", and so on..
library(readxl)
Port_Code <- read_excel("C:/Users/user/Documents/Day2/Portcodes Dictionary.xlsx")
library(dplyr)
df_Data <- merge(df_Data, Port_Code, by.x = "POD",by.y = "PORT CODE")
df_Data <- merge(df_Data, Port_Code, by.x = "POL",by.y = "PORT CODE")

## Remove duplicate records in the dataset by following fields
## "CNTR_N","CNTR_WGT_KG","POL","POD","LOAD_VSL_NAME","DISC_VSL_NAME"
df_Data[duplicated(df_Data[c("CNTR_N","CNTR_WGT_KG","POL","POD","LOAD_VSL_NAME","DISC_VSL_NAME")]),]
newdpdf_Data<-df_Data[!duplicated(df_Data[c("CNTR_N","CNTR_WGT_KG","POL","POD","LOAD_VSL_NAME","DISC_VSL_NAME")]),]

library(ggplot2)
## boxplot for one variable of CNTR_FOE and CNTR_WGT_KG from df_Data 
df_Data %>% ggplot(aes(CNTR_FOE, CNTR_WGT_KG)) + geom_boxplot()

df_Data %>% ggplot(aes(CNTR_WGT_KG)) + geom_histogram(binwidth = 500)

## mtcars
str(mtcars)
sapply(mtcars, function(x) sum(is.na(x)))
rowSums(is.na(mtcars))

mtcars2 <-within(mtcars, {
  vs <- factor(vs, labels = c("V","S"))
  am <- factor(am, labels = c("automatic","manual"))
  cyl <- ordered(cyl)
  gear <- ordered(gear)
  carb <- ordered(carb)
})
  
mtcars$am = factor(mtcars$am, labels = c("automatic","manual"))

str(mtcars2)
attach(mtcars2)
summary(mtcars2)
quantile(mpg)
IQR(mpg)
mean(mpg)
median(mpg)

quantile(mpg,prob = c(0.15,0.25,0.35))

unique(am)
match(am, unique(am))
tabulate(match(am,unique(am)))
which.max(tabulate(match(am,unique(am))))

#mode
#Create the function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
getmode(am)


var(mpg)
sd(mpg)

## ggplot(data=mtcars, aes(x=disp, y=mpg)) + geom_point(aes(size=hp, color=wt))

pairs(mtcars)
pairs(mtcars2[,c(1,3,4,5,6,7)])
cov(mpg,wt)
cor(mpg,wt)
cor.test(mpg,wt,method="pearson")

