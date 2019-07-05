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
    "car",
    "tseries",
    "forecast"
  )
)


library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)
library(stringr)
library(lubridate)
library(tseries)
library(forecast)
library(InformationValue)


setwd('C:/Users/martinelli/Documents/ADAPT PROGRAM/DAY 5/workspace/')

#Load the dataset adult

path = 'C:/Users/martinelli/Documents/ADAPT PROGRAM/DAY 5/workspace'
adultDataset = read.csv(paste(path,'adult.csv',sep='/'),check.names = FALSE)

class(adultDataset)
dim(adultDataset)
summary(adultDataset)
colnames(adultDataset)
str(adultDataset)
#Missing data columns
sapply(adultDataset, function(x) sum(is.na(x)))
#Check if data are numerics
unlist(lapply(adultDataset, is.numeric))
head(adultDataset)

adultData_df = tbl_df(adultDataset)

# checking di BIAS of data
# caluclate the frequency
# histogram of age by income group
table(adultDataset$ABOVE50K)
barplot(table(adultDataset$ABOVE50K), col="lightblue")

# CREATION ON TREANING DATA

input_ones <- adultDataset[which(adultDataset$ABOVE50K==1),]
input_zeros <- adultDataset[which(adultDataset$ABOVE50K==0),]

set.seed(100)

input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))

training_ones <- input_ones[input_ones_training_rows,]
training_zeros <- input_zeros[input_zeros_training_rows,]

trainingData <- rbind(training_ones,training_zeros)

barplot(table(trainingData$ABOVE50K), col="lightblue")
nrow(trainingData)/nrow(adultDataset)



#Create the TEST dataset, it will be used to test the model. It should reflect the reality, meaning
# both high and low income data should be as it exist int the popolation

test_ones <- input_ones[-input_ones_training_rows,]
test_zeros <- input_zeros[-input_zeros_training_rows,]
TestDataSet=rbind(test_ones,test_zeros)

barplot(table(TestDataSet$ABOVE50K), col="lightblue")

# THE TEST DATA = ALL DATA - TRAINING DATA
dim(TestDataSet)
dim(trainingData)
dim(adultDataset)

#Select all Significant Factors affetting the Income level
# Selecting the variables
factor_vars <-  c("WORKCLASS","EDUCATION","MARITALSTATUS","OCCUPATION",
                  "RELATIONSHIP","RACE","SEX","NATIVECOUNTRY")
continuous_vars <- c("AGE","FNLWGT", "EDUCATIONNUM","HOURSPERWEEK","CAPITALGAIN","CAPITALLOSS")

# initialization for the Information Variable result IV
iv_df <- data.frame(VARS = c(factor_vars,continuous_vars), IV=numeric(14)) # 14 è il numero delle variabili
iv_df # now it' s empty

#compute IV (Information value) for categorical Variables and the importance
iv_df[iv_df$VARS=="WORKCLASS","IV"] <- IV(X=adultDataset$WORKCLASS,Y=adultDataset$ABOVE50K)[1]
iv_df[iv_df$VARS=="EDUCATION","IV"] <- IV(X=adultDataset$EDUCATION,Y=adultDataset$ABOVE50K)[1]
iv_df[iv_df$VARS=="MARITALSTATUS","IV"] <- IV(X=adultDataset$MARITALSTATUS,Y=adultDataset$ABOVE50K)[1]
iv_df[iv_df$VARS=="OCCUPATION","IV"] <- IV(X=adultDataset$OCCUPATION,Y=adultDataset$ABOVE50K)[1]
iv_df[iv_df$VARS=="RACE","IV"] <- IV(X=adultDataset$RACE,Y=adultDataset$ABOVE50K)[1]
iv_df[iv_df$VARS=="SEX","IV"] <- IV(X=adultDataset$SEX,Y=adultDataset$ABOVE50K)[1]
iv_df[iv_df$VARS=="NATIVECOUNTRY","IV"] <- IV(X=adultDataset$NATIVECOUNTRY,Y=adultDataset$ABOVE50K)[1]

#compute IV (Information value) for continous Variables
iv_df[iv_df$VARS=="AGE","IV"] <- IV(X=adultDataset$AGE,Y=adultDataset$ABOVE50K)[1]
iv_df[iv_df$VARS=="FNLWGT","IV"] <- IV(X=adultDataset$FNLWGT,Y=adultDataset$ABOVE50K)[1]
iv_df[iv_df$VARS=="EDUCATIONNUM","IV"] <- IV(X=adultDataset$EDUCATIONNUM,Y=adultDataset$ABOVE50K)[1]
iv_df[iv_df$VARS=="HOURSPERWEEK","IV"] <- IV(X=adultDataset$HOURSPERWEEK,Y=adultDataset$ABOVE50K)[1]
iv_df[iv_df$VARS=="CAPITALGAIN","IV"] <- IV(X=adultDataset$CAPITALGAIN,Y=adultDataset$ABOVE50K)[1]
iv_df[iv_df$VARS=="CAPITALLOSS","IV"] <- IV(X=adultDataset$CAPITALLOSS,Y=adultDataset$ABOVE50K)[1]

#Build the logistic regression --> Modelling
logitMod <- 
glm(ABOVE50K ~ MARITALSTATUS + AGE + OCCUPATION + EDUCATION + HOURSPERWEEK + CAPITALGAIN + SEX, data=trainingData, family=binomial(link="logit"))

#predicted scores
predicted <- predict(logitMod, TestDataSet, type="response")
summary(logitMod)

#Evaluation with the consufes matrix
cm <- confusionMatrix(logitMod, predicted)
plotROC(TestDataSet$ABOVE50K, predicted)
fourfoldplot(as.matrix(cm))
