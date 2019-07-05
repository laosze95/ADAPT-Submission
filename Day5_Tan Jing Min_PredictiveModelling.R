#load data file (set AdultData as name of the file)
AdultData <- read.csv(file="C:/Users/jingmin/Desktop/ADAPT-master/ADAPT-master/adult.csv", header=TRUE, sep=",")
View(AdultData)

#summary
str(AdultData)
summary(AdultData)

#check how many rows and columns 
dim(AdultData)

#check class
class(AdultData)

#check any NA
sum(is.na(AdultData))

library(InformationValue)

#check the bias of the data 
table(AdultData$ABOVE50K)
#can see data is biased more on people not above 50k
#see by barplot 
barplot(table(AdultData$ABOVE50K),col ='lightblue')

#how to pick up training set of data 
#create training dataset which is unbiased 

input_ones <- AdultData[which(AdultData$ABOVE50K == 1),]
input_zeroes <- AdultData[which(AdultData$ABOVE50K == 0),]

#for repeatability of samples
set.seed(100)

#1s for training
input_ones_training_rows <- sample(1:nrow(input_ones),0.7*nrow(input_ones))

#0s for training, need to make sure is input_ones as this will equalize the 0s. 
input_zeroes_training_rows <- sample(1:nrow(input_zeroes),0.7*nrow(input_ones))

#pick as many 0s and 1s
training_ones <- input_ones[input_ones_training_rows,]
training_zeros <- input_zeroes[input_zeroes_training_rows,]

#picking the dataset and combining as 1
trainingData <- rbind(training_ones,training_zeros)

summary(trainingData)

#check the bias of the data 
table(trainingData$ABOVE50K)

#see by barplot 
barplot(table(trainingData$ABOVE50K),col ='lightblue')

#create the test dataset
#test dataset will be used to test the model. it should reflect the reality, meaning both high and low income data should be as it exist in environment
#test dataset will be initial dataset exclude training dataset 

#pick as many 0s and 1s (EXCLUDE the training ones rows and zeroes)
testing_ones <- input_ones[-input_ones_training_rows,]
testing_zeros <- input_zeroes[-input_zeroes_training_rows,]

#picking the dataset and combining as 1
TestingData <- rbind(testing_ones,testing_zeros)
#do not need to test bias as it should include future uncertainty 

summary(TestingData)
dim(TestingData)
#do a count 
dim(TestingData)
dim(AdultData)
dim(TrainingData)
#see if rows tally 

#difference between testing & training data (both will add to 100%)
#only training data can be used to build the model
#after model is built, then will go into testing 
#when testing, only can use test dataset, only then, can get accuracy
#why cant test on training set, cause model is built on training data, if test on training data, accuracy will be confirm high
#testdata usually between 10% to 30% of data 
#in more advanced, there is 1 more called validation set 
#creating validation set is due to sometimes fine tuning of model include test data, then pass the model through validation set to get accuracy

#select the significant factors affecting the income level
#this is also called feature engineering to find out most significant factors affecting the outcome 

library(InformationValue)

#segreate continous and factor variables
factor_vars <- c("WORKCLASS","EDUCATION","MARITALSTATUS","OCCUPATION","RELATIONSHIP","RACE","SEX","NATIVECOUNTRY")
continous_vars <- c("AGE","FNLWGT","EDUCATIONNUM","HOURSPERWEEK","CAPITALGAIN","CAPITALLOSS")

#initialisation for the for IV results
iv_df <- data.frame(VARS=c(factor_vars,continous_vars), IV=numeric(14))
View(iv_df)

#compute IV for categorical variables 
iv_df[iv_df$VARS == "WORKCLASS","IV"] <- IV(X=AdultData$WORKCLASS,Y=AdultData$ABOVE50K)[1]
iv_df[iv_df$VARS == "EDUCATION","IV"] <- IV(X=AdultData$EDUCATION,Y=AdultData$ABOVE50K)[1]
iv_df[iv_df$VARS == "OCCUPATION","IV"] <- IV(X=AdultData$OCCUPATION,Y=AdultData$ABOVE50K)[1]
iv_df[iv_df$VARS == "MARITALSTATUS","IV"] <- IV(X=AdultData$MARITALSTATUS,Y=AdultData$ABOVE50K)[1]
iv_df[iv_df$VARS == "RACE","IV"] <- IV(X=AdultData$RACE,Y=AdultData$ABOVE50K)[1]
iv_df[iv_df$VARS == "SEX","IV"] <- IV(X=AdultData$SEX,Y=AdultData$ABOVE50K)[1]
iv_df[iv_df$VARS == "NATIVECOUNTRY","IV"] <- IV(X=AdultData$NATIVECOUNTRY,Y=AdultData$ABOVE50K)[1]
View(iv_df)
iv_df[iv_df$VARS == "RELATIONSHIP","IV"] <- IV(X=AdultData$RELATIONSHIP,Y=AdultData$ABOVE50K)[1]
View(iv_df)

#compute IV for factor variables 
iv_df[iv_df$VARS == "AGE","IV"] <- IV(X=as.factor(AdultData$AGE),Y=AdultData$ABOVE50K)[1]
iv_df[iv_df$VARS == "FNLWGT","IV"] <- IV(X=as.factor(AdultData$FNLWGT),Y=AdultData$ABOVE50K)[1]
iv_df[iv_df$VARS == "EDUCATIONNUM","IV"] <- IV(X=as.factor(AdultData$EDUCATIONNUM),Y=AdultData$ABOVE50K)[1]
iv_df[iv_df$VARS == "HOURSPERWEEK","IV"] <- IV(X=as.factor(AdultData$HOURSPERWEEK),Y=AdultData$ABOVE50K)[1]
iv_df[iv_df$VARS == "CAPITALGAIN","IV"] <- IV(X=as.factor(AdultData$CAPITALGAIN),Y=AdultData$ABOVE50K)[1]
iv_df[iv_df$VARS == "CAPITALLOSS","IV"] <- IV(X=as.factor(AdultData$CAPITALLOSS),Y=AdultData$ABOVE50K)[1]

logitMod <- glm(formula = ABOVE50K ~ MARITALSTATUS + AGE + OCCUPATION + EDUCATION + HOURSPERWEEK + CAPITALGAIN + SEX, data = trainingData, family = binomial(link = "logit"))
predicted <- predict(logitMod, TestingData, type = "response")
summary(logitMod)

##more stars show it is more significant 

plotROC(TestingData$ABOVE50K, predicted)
#area under the curve 

##CONFUSION MATRIX
colnames(cm) <- c("Actual 0","Actual 1")
rownames(cm) <- c("Predicted 0", "Predicted 1")
fourfoldplot(as.matrix(cm))