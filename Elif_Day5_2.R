setwd("C:/Users/edursun/Desktop/ADAPT/ADAPT_4")
adult <- read.csv(file="C:/Users/edursun/Desktop/ADAPT/ADAPT_4/adult.csv",
                    header=TRUE, sep=",")
dim(adult)
str(adult)
summary(adult)
library(InformationValue)
head(adult)
adult2 <- tbl_df(adult)

#veri bias mý deðil mi diye bakýyoruz.
table(adult$ABOVE50K)
barplot(table(adult$ABOVE50K),col="lightblue")


input_ones<- adult[which(adult$ABOVE50K==1),] #all 1
input_zeros<- adult[which(adult$ABOVE50K==0),] #all 1

set.seed(100) # for repeatability

# training için eþit sayýda örnek seçmeye çalýþýyoruz bu yüzden %70 i alýndý.
input_ones_training_rows <- sample(1:nrow(input_ones),0.7*nrow(input_ones)) #1 s for training
input_zeros_training_rows <- sample(1:nrow(input_zeros),0.7*nrow(input_ones))  #0 s for training

#pick as many 0's as 1's

training_ones <- input_ones[input_ones_training_rows,]
training_zeros <- input_zeros[input_zeros_training_rows,]
training_Data <- rbind(training_ones,training_zeros)
nrow(training_ones)
nrow(training_zeros)

# en son seçtiðimiz örnektekiler eþit mi diye tekrar kontrol ediyoruz
barplot(table(training_Data$ABOVE50K),col="lightblue")

#test dataset

testing_ones <- input_ones[-input_ones_training_rows,]
testing_zeros <- input_zeros[-input_zeros_training_rows,]
testing_Data <- rbind(testing_ones,testing_zeros)
barplot(table(testing_Data$ABOVE50K),col="lightblue")

#selecting the significant factors affecting the ýncome level
#segregate continous and factor variables

factor_vars <- c("WORKCLASS", "EDUCATION", "MARITALSTATUS","OCCUPATION","RELATIONSHIP","RACE","SEX","NATIVECOUNTRY")
continous_vars <- c("AGE","FNLWGT","EDUCATIONNUM","HOURSPERWEEK","CAPITALGAIN","CAPITALLOSS")

#initialization for the for IV results
iv_df <- data.frame(VARS=c(factor_vars,continous_vars), IV=numeric(14))

#compute IV for categorical variables

iv_df[iv_df$VARS=="WORKCLASS","IV"] <- IV(X=adult$WORKCLASS,Y=adult$ABOVE50K) [1]
iv_df[iv_df$VARS=="EDUCATION","IV"] <- IV(X=adult$EDUCATION,Y=adult$ABOVE50K) [1]
iv_df[iv_df$VARS=="MARITALSTATUS","IV"] <- IV(X=adult$MARITALSTATUS,Y=adult$ABOVE50K) [1]
iv_df[iv_df$VARS=="OCCUPATION","IV"] <- IV(X=adult$OCCUPATION,Y=adult$ABOVE50K) [1]
iv_df[iv_df$VARS=="RELATIONSHIP","IV"] <- IV(X=adult$RELATIONSHIP,Y=adult$ABOVE50K) [1]
iv_df[iv_df$VARS=="RACE","IV"] <- IV(X=adult$RACE,Y=adult$ABOVE50K) [1]
iv_df[iv_df$VARS=="SEX","IV"] <- IV(X=adult$SEX,Y=adult$ABOVE50K) [1]
iv_df[iv_df$VARS=="NATIVECOUNTRY","IV"] <- IV(X=adult$NATIVECOUNTRY,Y=adult$ABOVE50K) [1]


#compute IV for continous variables

iv_df[iv_df$VARS=="AGE","IV"] <- IV(X=as.factor(adult$AGE),Y=adult$ABOVE50K) [1]
iv_df[iv_df$VARS=="FNLWGT","IV"] <- IV(X=as.factor(adult$FNLWGT),Y=adult$ABOVE50K) [1]
iv_df[iv_df$VARS=="EDUCATIONNUM","IV"] <- IV(X=as.factor(adult$EDUCATIONNUM),Y=adult$ABOVE50K) [1]
iv_df[iv_df$VARS=="HOURSPERWEEK","IV"] <- IV(X=as.factor(adult$HOURSPERWEEK),Y=adult$ABOVE50K) [1]
iv_df[iv_df$VARS=="CAPITALGAIN","IV"] <- IV(X=as.factor(adult$CAPITALGAIN),Y=adult$ABOVE50K) [1]
iv_df[iv_df$VARS=="CAPITALLOSS","IV"] <- IV(X=as.factor(adult$CAPITALLOSS),Y=adult$ABOVE50K) [1]

#Logistics regression

LogitMod <- glm(ABOVE50K~MARITALSTATUS+AGE+OCCUPATION+EDUCATION+HOURSPERWEEK+CAPITALGAIN+SEX,data = training_Data )

#predicted scores
predicted<- predict(LogitMod,testing_Data,type = "response")
View(predicted)
summary(LogitMod)

plotROC(testing_Data$ABOVE50K,predicted)

#confusion matrix

cm <- confusionMatrix(testing_Data$ABOVE50K,predicted)
colnames(cm) <- c("Actual 0", "Actual 1")
rownames (cm)<- c("Predicted 0", "Predicted 1")
fourfoldplot(as.matrix(cm))