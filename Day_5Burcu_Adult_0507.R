# new data frame from begining
Adultt = read.csv('C:/Users/btutar/Desktop/ADAPT-master/adult.csv')
View(Adultt)
dim(Adultt)
summary(Adultt)
str(Adultt)
class(Adultt)
View(summary(Adultt))
head(Adultt)

# number and histogram how many people income above 50k or not
table(Adultt$ABOVE50K)

barplot(table(Adultt$ABOVE50K), col= "yellow")

# all 1's
input_ones<- Adultt[which(Adultt$ABOVE50K==1), ]

# all 0's
input_zeros<- Adultt[which(Adultt$ABOVE50K==0), ]

#for repeatability of samples
set.seed(100)

# 1's for tarining
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))

# 0's for tarining
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))

# pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]
training_zeros <- input_zeros[input_zeros_training_rows, ]

trainingData <- rbind(training_ones,training_zeros)

# checking 0's and 1's are equal
View(trainingData)
barplot(table(trainingData$ABOVE50K), col= "pink")

#creating testing data
testing_ones <- input_ones[-input_ones_training_rows,]
testing_zeros <- input_zeros[-input_zeros_training_rows,]

testingData <- rbind(testing_ones,testing_zeros)

View(testingData)
table(testingData$ABOVE50K)
barplot(table(testingData$ABOVE50K), col= "pink")

# information value function segregate continous and factor variables

library(InformationValue)
factor_vars <- c("WORKCLASS", "EDUCATION","MARITALSTATUS","OCCUPATION",
                 "RELATIONSHIP","RACE","SEX","NATIVECOUNTRY")
continuous_vars <- c("AGE","FNLWGT","EDUCATIONNUM","HOURSPERWEEK","CAPITALAGAIN","CAPITALLOSS")


# initialization for thr for IV results
iv_df <- data.frame(VARS=c(factor_vars,continuous_vars), IV=numeric(14))

View(iv_df)

# compute IV for categorical variable
iv_df[iv_df$VARS == "WORKCLASS", "IV"] <- IV(X=inputData$WORKCLASS,
                                             Y= inputData$ABOVE50K) [1]

iv_df[iv_df$VARS == "EDUCATION", "IV"] <- IV(X=inputData$EDUCATION,
                                             Y= inputData$ABOVE50K) [1]

iv_df[iv_df$VARS == "MARITALSTATUS", "IV"] <- IV(X=inputData$MARITALSTATUS,
                                             Y= inputData$ABOVE50K) [1]

iv_df[iv_df$VARS == "OCCUPATION", "IV"] <- IV(X=inputData$OCCUPATION,
                                             Y= inputData$ABOVE50K) [1]

iv_df[iv_df$VARS == "RACE", "IV"] <- IV(X=inputData$RACE,
                                             Y= inputData$ABOVE50K) [1]

iv_df[iv_df$VARS == "SEX", "IV"] <- IV(X=inputData$SEX,
                                             Y= inputData$ABOVE50K) [1]

iv_df[iv_df$VARS == "NATIVECOUNTRY", "IV"] <- IV(X=inputData$NATIVECOUNTRY,
                                             Y= inputData$ABOVE50K) [1]

# COMPUT IV for continous variables

iv_df[iv_df$VARS == "AGE", "IV"] <- IV(X=as.factor(inputData$AGE),
                                                 Y= inputData$ABOVE50K) [1]

iv_df[iv_df$VARS == "EDUCATIONNUM", "IV"] <- IV(X=as.factor(inputData$EDUCATIONNUM),
                                       Y= inputData$ABOVE50K) [1]

iv_df[iv_df$VARS == "HOURSPERWEEK", "IV"] <- IV(X=as.factor(inputData$HOURSPERWEEK),
                                       Y= inputData$ABOVE50K) [1]

iv_df[iv_df$VARS == "CAPITALAGAIN", "IV"] <- IV(X=as.factor(inputData$CAPITALAGAIN),
                                       Y= inputData$ABOVE50K) [1]


iv_df[iv_df$VARS == "CAPITALLOSS", "IV"] <- IV(X=as.factor(inputData$CAPITALLOSS),
                                                Y= inputData$ABOVE50K) [1]

#Modelling

iv_df <- iv_df[order(-iv_df$IV), ]
iv_df

logitMod <- glm(ABOVE50K ~ MARITALSTATUS + AGE + OCCUPATION +
                  EDUCATION + HOURSPERWEEK + CAPITALGAIN + SEX, data=trainingData,
                family=binomial(link="logit"))

#PREDICTED SCORES

predicted <- predict(logitMod, testingData, type = "response")

summary(logitMod)

#testing and validation
plotROC(testingData$ABOVE50K, predicted)

# 

colnames(cm) <- c("Actual 0", "Actual 1")
rownames(cm) <- c("Predicted 0", "Predicted 1")
fourfoldplot(as.matrix(cm))



