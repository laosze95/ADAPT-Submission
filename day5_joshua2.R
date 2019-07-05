#load data
adult = read.csv("adult.csv")
View(adult)

#get info on dataset
str(adult)
summary(adult)

table(adult$ABOVE50K)

barplot(table(adult$ABOVE50K), col = "lightblue")

input_ones <- adult[which(adult$ABOVE50K ==1),]
input_zeros <- adult[which(adult$ABOVE50K ==0),]

set.seed(100)

#training data
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))

#selecting the training data
training_ones <- input_ones[input_ones_training_rows,]
training_zeros <- input_zeros[input_zeros_training_rows,]

trainingData <- rbind(training_ones, training_zeros)

barplot(table(trainingData$ABOVE50K), col="lightblue")

#those not used for training, will be used for testing
testing_ones <- input_ones[-input_ones_training_rows,]
testing_zeros <- input_zeros[-input_zeros_training_rows,]

testingdata <- rbind(testing_ones, testing_zeros)

barplot(table(testingdata$ABOVE50K), col="blue")

factor_vars <- c("WORKCLASS", "EDUCATION", "MARITALSTATUS", "OCCUPATION", "RELATIONSHIP", "RACE", "SEX", "NATIVECOUNTRY")

continuous_vars <- c("AGE", "FNLWGT", "EDUCATIONNUM", "HOURSPERWEEK", "CAPTIALGAIN", "CAPITALLOSS")
iv_df <- data.frame(VARS = c(factor_vars, continuous_vars), IV=numeric(14))

iv_df[iv_df$VARS == "WORKCLASS", "IV"] <- IV(X=adult$WORKCLASS, Y=adult$ABOVE50K)[1]
iv_df[iv_df$VARS == "EDUCATION", "IV"] <- IV(X=adult$EDUCATION, Y=adult$ABOVE50K)[1]
iv_df[iv_df$VARS == "MARITALSTATUS", "IV"] <- IV(X=adult$MARITALSTATUS, Y=adult$ABOVE50K)[1]
iv_df[iv_df$VARS == "OCCUPATION", "IV"] <- IV(X=adult$OCCUPATION, Y=adult$ABOVE50K)[1]
iv_df[iv_df$VARS == "RACE", "IV"] <- IV(X=adult$RACE, Y=adult$ABOVE50K)[1]
iv_df[iv_df$VARS == "SEX", "IV"] <- IV(X=adult$SEX, Y=adult$ABOVE50K)[1]
iv_df[iv_df$VARS == "NATIVECOUNTRY", "IV"] <- IV(X=adult$NATIVECOUNTRY, Y=adult$ABOVE50K)[1]
iv_df[iv_df$VARS == "AGE", "IV"] <- IV(X=as.factor(adult$AGE), Y=adult$ABOVE50K)[1]
iv_df[iv_df$VARS == "EDUCATIONNUM", "IV"] <- IV(X=as.factor(adult$EDUCATIONNUM), Y=adult$ABOVE50K)[1]
iv_df[iv_df$VARS == "HOURSPERWEEK", "IV"] <- IV(X=as.factor(adult$HOURSPERWEEK), Y=adult$ABOVE50K)[1]
iv_df[iv_df$VARS == "CAPTIALGAIN", "IV"] <- IV(X=as.factor(adult$CAPITALGAIN), Y=adult$ABOVE50K)[1]
iv_df[iv_df$VARS == "CAPTIALLOSS", "IV"] <- IV(X=as.factor(adult$CAPITALLOSS), Y=adult$ABOVE50K)[1]

logitMod <- glm(ABOVE50K ~ MARITALSTATUS + AGE + OCCUPATION + EDUCATION + HOURSPERWEEK + CAPITALGAIN + SEX, data = trainingData, family = binomial(link = 'logit'))

predicted <- predict(logitMod, testingdata, type = "response")
summary(logitMod)

plotROC(testingdata$ABOVE50K, predicted)

colnames(cm) <- c("Actual 0", "Actual 1")
rownames(cm) <- c("Predicted 0", "Predicted 1")
fourfoldplot(as.matrix(cm))


