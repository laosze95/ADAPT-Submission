#Javier Kan Day 5 Exercise Adult dataset
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

View(df_Data)

#Explore dataset
str(df_Data)
dim(df_Data)
colSums(is.na(df_Data))

#check the number of adults above 50k 
table(df_Data$ABOVE50K)
barplot(table(df_Data$ABOVE50K), col ="red")

input_ones <- df_Data[which(df_Data$ABOVE50K == 1),] #all 1
input_zeros <- df_Data[which(df_Data$ABOVE50K == 0),] #all 0

set.seed(100) #For repeatability of samples

input_ones_training_rows <- sample(1:nrow(input_ones),0.7*nrow(input_ones))
#1 for training
input_zeros_training_rows <- sample(1:nrow(input_zeros),0.7*nrow(input_ones))
#0 for training

#Pick as many 0 and 1
training_ones <- input_ones[input_ones_training_rows, ]
training_zeros <- input_zeros[input_zeros_training_rows, ]

trainingData <- rbind(training_ones,training_zeros)
barplot(table(trainingData$ABOVE50K), col ="red")

#Test remaining data
testing_ones <- input_ones[-input_ones_training_rows, ]
testing_zeros <- input_zeros[-input_zeros_training_rows, ]
Combinedtestingtable <-rbind(testing_ones, testing_zeros)
barplot(table(Combinedtestingtable$ABOVE50K), col ="red")
table(Combinedtestingtable$ABOVE50K)

#Segregate continuous and factorvariables
factor_vars <- c("WORKCLASS","EDUCATION","MARITIALSTATUS","OCCUPATION","RELATIONSHIP","RACE","SEX","NATIVECOUNTRY")
continuous_vars <- c("AGE","FNLWGT","EDUCATIONNUM","HOURSPERWEEK","CAPITALGAIN","CAPITALLOSS")

#Initialization for the IV result
iv_df <- data.frame(VARS=c(factor_vars,continuous_vars), IV = numeric(14))

#Compute the IV for categorical variables
iv_df[iv_df$VARS == "WORKCLASS","IV"] <- IV(X=df_Data$WORKCLASS,Y=df_Data$ABOVE50K)[1]
iv_df[iv_df$VARS == "EDUCATION","IV"] <- IV(X=df_Data$EDUCATION,Y=df_Data$ABOVE50K)[1]
iv_df[iv_df$VARS == "MARITIALSTATUS","IV"] <- IV(X=df_Data$MARITALSTATUS,Y=df_Data$ABOVE50K)[1]
iv_df[iv_df$VARS == "OCCUPATION","IV"] <- IV(X=df_Data$OCCUPATION,Y=df_Data$ABOVE50K)[1]
iv_df[iv_df$VARS == "RACE","IV"] <- IV(X=df_Data$RACE,Y=df_Data$ABOVE50K)[1]
iv_df[iv_df$VARS == "SEX","IV"] <- IV(X=df_Data$SEX,Y=df_Data$ABOVE50K)[1]
iv_df[iv_df$VARS == "NATIVECOUNTRY","IV"] <- IV(X=df_Data$NATIVECOUNTRY,Y=df_Data$ABOVE50K)[1]

#COmpute the IV for continuous variables
iv_df[iv_df$VARS == "AGE","IV"] <- IV(X=as.factor(df_Data$AGE),Y=df_Data$ABOVE50K)[1]
iv_df[iv_df$VARS == "EDUCATIONNUM","IV"] <- IV(X=as.factor(df_Data$EDUCATIONNUM),Y=df_Data$ABOVE50K)[1]
iv_df[iv_df$VARS == "HOURSPERWEEK","IV"] <- IV(X=as.factor(df_Data$HOURSPERWEEK),Y=df_Data$ABOVE50K)[1]
iv_df[iv_df$VARS == "CAPITALGAIN","IV"] <- IV(X=as.factor(df_Data$CAPITALGAIN),Y=df_Data$ABOVE50K)[1]
iv_df[iv_df$VARS == "CAPITALLOSS","IV"] <- IV(X=as.factor(df_Data$CAPITALLOSS),Y=df_Data$ABOVE50K)[1]

iv_df <- iv_df[order(-iv_df$IV),]
iv_df

logitMod <- glm(ABOVE50K ~ MARITALSTATUS + AGE + OCCUPATION + EDUCATION + HOURSPERWEEK + CAPITALGAIN + SEX, data = trainingData,
family = binomial(link="logit"))

#Predicted scores
predicted <- predict(logitMod, Combinedtestingtable, type="response")
summary(logitMod)

plotROC(Combinedtestingtable$ABOVE50K, predicted)



