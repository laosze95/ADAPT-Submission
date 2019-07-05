inputData <- read.csv("C:/Users/user/Documents/Day2/adult.CSV");

View(inputData)
str(inputData)
dim(inputData)
summary(inputData)
head(inputData)

table(inputData$ABOVE50K)
## historam of age by income group
barplot(table(inputData$ABOVE50K),col="lightblue")

input_ones <- inputData[which(inputData$ABOVE50K == 1),]
input_zeros <- inputData[which(inputData$ABOVE50K == 0),]

set.seed(100)

input_ones_training_rows <- sample(1:nrow(input_ones),0.7*nrow(input_ones)) ## 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros),0.7*nrow(input_ones)) ## 0's for training

##Pick as mnay 0's as 1's
training_ones <- input_ones[input_ones_training_rows,]
training_Zeros <- input_zeros[input_zeros_training_rows,]

trainingData <- rbind(training_ones, training_Zeros)

## Checking the bias on training data
barplot(table(trainingData$ABOVE50K),col="lightblue")

nrow(trainingData)/nrow(inputData)

## Create test data without training data
test_ones <- input_ones[-input_ones_training_rows,]
test_zeros <- input_zeros[-input_zeros_training_rows,]
test_data <- rbind(test_ones, test_zeros)
barplot(table(test_data$ABOVE50K),col="lightblue")

## select the significant factors affecting the income level
# segregate continuoos and factor variables
factor_vars <- c("WORKCLASS","EDUCATION","MARITALSTATUS","OCCUPATION","RELATIONSHIP","RACE","SEX","NATIVECOUNTRY")
CONTINUOUS_VARS <- c("AGE","FNLWGT","EDUCATIONNUM","HOURSPERWEEK","CAPITALGAIN","CAPITALLOSS")
# initialization for the for IV results
iv_df <- data.frame(VARS=c(factor_vars, CONTINUOUS_VARS), IV=numeric(14))
iv_df
# compute IV for categorical Variables
iv_df[iv_df$VARS == "WORKCLASS","IV"] <- IV(X=inputData$WORKCLASS,Y=inputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "EDUCATION","IV"] <- IV(X=inputData$EDUCATION,Y=inputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "MIRITALSTATUS","IV"] <- IV(X=inputData$MARITALSTATUS,Y=inputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "OCCUPATION","IV"] <- IV(X=inputData$OCCUPATION,Y=inputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "RELATIONSHIP","IV"] <- IV(X=inputData$RELATIONSHIP,Y=inputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "RACE","IV"] <- IV(X=inputData$RACE,Y=inputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "SEX","IV"] <- IV(X=inputData$SEX,Y=inputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "NATIVECOUNTRY","IV"] <- IV(X=inputData$NATIVECOUNTRY,Y=inputData$ABOVE50K)[1]

iv_df[iv_df$VARS == "AGE","IV"] <- IV(X=as.factor(inputData$AGE),Y=inputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "FNLWGT","IV"] <- IV(X=as.factor(inputData$FNLWGT),Y=inputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "EDUCATIONNUM","IV"] <- IV(X=as.factor(inputData$EDUCATIONNUM),Y=inputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "HOURSPERWEEK","IV"] <- IV(X=as.factor(inputData$HOURSPERWEEK),Y=inputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "CAPITALGAIN","IV"] <- IV(X=as.factor(inputData$CAPITALGAIN),Y=inputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "CAPITALLOSS","IV"] <- IV(X=as.factor(inputData$CAPITALLOSS),Y=inputData$ABOVE50K)[1]

logitMod <- glm(ABOVE50K ~ MARITALSTATUS + AGE + OCCUPATION + EDUCATION + HOURSPERWEEK + CAPITALGAIN + SEX, data = trainingData)
family = binomial(link = "logit")

## predicated scores
predicted <- predict(logitMod, test_data, type = "response")
summary(logitMod)

plotROC(test_data$ABOVE50K, predicted)


