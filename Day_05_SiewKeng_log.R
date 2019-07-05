library(dplyr)
library(readxl)
library(ggplot2)
library(reshape2)
library(stringr)
library(lubridate)
library(data.table)
library(InformationValue)
library(tseries)
library(forecast)


setwd("D:/ADAPT")
df_Data = data.frame()

path="D:/ADAPT"
#load ADAPT_Sample_dataset*.csv into df_Data 
myfiles = dir(path)

for (file in myfiles)
{
  if(grepl("csv",file))
  {
    indata<- fread(file,check.names = FALSE, stringsAsFactors = FALSE)
    df_Data<-rbind(df_Data,indata)
  }
  
}



#EDA
str(df_Data)
sapply(df_Data, class)
summary(df_Data)

table(df_Data$ABOVE50K)
barplot(table(df_Data$ABOVE50K), col = 'lightblue')




#create testing data with balance of ABOVE50K
df_Data_1 = df_Data[which(df_Data$ABOVE50K == 1),]
df_Data_0 = df_Data[which(df_Data$ABOVE50K == 0),]

set.seed(100)

#70% of the smallest set
training_1_row  <- sample(1:nrow(df_Data_1),0.7*nrow(df_Data_1)) 
training_0_row  <- sample(1:nrow(df_Data_0),0.7*nrow(df_Data_1))

df_training_1 <- df_Data_1[training_1_row,]
df_training_0 <- df_Data_0[training_0_row,]

df_training <- rbind(df_training_1,df_training_0)

barplot(table(df_training$ABOVE50K), col = 'lightblue')


#create test data
df_test_1 <- df_Data_1[-training_1_row,]
df_test_0 <- df_Data_0[-training_0_row,]
#df_test_1 <- df_Data_1[!training_1_row,]
#df_test_0 <- df_Data_0[!training_0_row,]

df_test <- rbind(df_test_1,df_test_0)

barplot(table(df_test$ABOVE50K), col = 'lightblue')


#factors eng

factor_vars <- c("WORKCLASS", "EDUCATION","MARITALSTATUS",
                "OCCUPATION","RELATIONSHIP","RACE","SEX","NATIVECOUNTRY")

continuous_vars <-c("AGE","FNLWGT","EDUCATIONNUM", "HOURSPERWEEK",
                    "CAPITALGAIN","CAPITALLOSS")

iv_df <- data.frame(VARS=c(factor_vars,continuous_vars), IV = numeric(14))



iv_df[iv_df$VARS == "WORKCLASS","IV"] <- IV(X=df_Data$WORKCLASS,Y=df_Data$ABOVE50K)[1]
iv_df[iv_df$VARS == "EDUCATION","IV"] <- IV(X=df_Data$EDUCATION,Y=df_Data$ABOVE50K)[1] 
iv_df[iv_df$VARS == "MARITALSTATUS","IV"] <- IV(X=df_Data$MARITALSTATUS,Y=df_Data$ABOVE50K)[1] 
iv_df[iv_df$VARS == "OCCUPATION","IV"] <- IV(X=df_Data$OCCUPATION,Y=df_Data$ABOVE50K)[1] 
iv_df[iv_df$VARS == "RELATIONSHIP","IV"] <- IV(X=df_Data$RELATIONSHIP,Y=df_Data$ABOVE50K)[1] 
iv_df[iv_df$VARS == "RACE","IV"] <- IV(X=df_Data$RACE,Y=df_Data$ABOVE50K)[1] 
iv_df[iv_df$VARS == "SEX","IV"] <- IV(X=df_Data$SEX,Y=df_Data$ABOVE50K)[1] 
iv_df[iv_df$VARS == "NATIVECOUNTRY","IV"] <- IV(X=df_Data$NATIVECOUNTRY,Y=df_Data$ABOVE50K)[1] 

iv_df[iv_df$VARS == "AGE","IV"] <- IV(X=df_Data$AGE,Y=df_Data$ABOVE50K)[1] 
iv_df[iv_df$VARS == "FNLWGT","IV"] <- IV(X=df_Data$FNLWGT,Y=df_Data$ABOVE50K)[1] 
iv_df[iv_df$VARS == "EDUCATIONNUM","IV"] <- IV(X=df_Data$EDUCATIONNUM,Y=df_Data$ABOVE50K)[1] 
iv_df[iv_df$VARS == "HOURSPERWEEK","IV"] <- IV(X=df_Data$HOURSPERWEEK,Y=df_Data$ABOVE50K)[1] 
iv_df[iv_df$VARS == "CAPITALGAIN","IV"] <- IV(X=df_Data$CAPITALGAIN,Y=df_Data$ABOVE50K)[1] 
iv_df[iv_df$VARS == "CAPITALLOSS","IV"] <- IV(X=df_Data$CAPITALLOSS,Y=df_Data$ABOVE50K)[1] 

#log model
logitMod <- glm(ABOVE50K ~ MARITALSTATUS + AGE + OCCUPATION 
                + EDUCATION + HOURSPERWEEK + CAPITALGAIN + SEX,
                data = df_training, family = binomial(link = "logit"))

predicted <- predict(logitMod,df_test,type='response')
summary(logitMod)

plotROC(df_test$ABOVE50K,predicted)


