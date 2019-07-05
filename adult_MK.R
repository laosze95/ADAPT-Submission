
InputData = read.csv("C:/Users/kmichael/Desktop/ADAPT data/adult.csv")

colnames(InputData)
table(InputData$ABOVE50K)
barplot(table(InputData$ABOVE50K))

input_ones = InputData[which(InputData$ABOVE50K == 1), ] #all 1's
input_zeros = InputData[which(InputData$ABOVE50K ==  0), ] #all 0's

set.seed(100) #for repeatability of samples

input_ones_training_rows = sample(1:nrow(input_ones), 0.7*nrow(input_ones))
#1's for training
input_zeros_training_rows = sample(1:nrow(input_zeros), 0.7*nrow(input_ones))
#0's for training

training_ones = input_ones[input_ones_training_rows, ]
training_zero = input_zeros[input_zeros_training_rows, ]

trainingData = rbind(training_ones,training_zero)

barplot(table(trainingData$ABOVE50K), col = "red")
table(trainingData$ABOVE50K)

test_ones = input_ones[-input_ones_training_rows, ]
test_zero = input_zeros[-input_zeros_training_rows, ]

testData = rbind(test_ones,test_zero)
table(testData$ABOVE50K)

#segregate the continuous and factor variables
factor_vars = c("WORKCLASS", "EDUCATION", "MARITALSTATUS", "OCCUPATION", 
                "RELATIONSHIP", "RACE", "SEX", "NATIVECOUNTRY")
continuous_vars = c("AGE", "FNLWGT", "EDUCATIONNUM", "HOURSPERWEEK",
                    "CAPITALGAIN", "CAPITALLOSS")

iv_df = data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(14))

iv_df[iv_df$VARS == "WORKCLASS", "IV" ] = IV(X=InputData$WORKCLASS, Y=InputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "EDUCATION", "IV" ] = IV(X=InputData$EDUCATION, Y=InputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "MARITALSTATUS", "IV" ] = IV(X=InputData$MARITALSTATUS, Y=InputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "OCCUPATION", "IV" ] = IV(X=InputData$OCCUPATION, Y=InputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "RELATIONSHIP", "IV" ] = IV(X=InputData$RELATIONSHIP, Y=InputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "RACE", "IV" ] = IV(X=InputData$RACE, Y=InputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "SEX", "IV" ] = IV(X=InputData$SEX, Y=InputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "NATIVECOUNTRY", "IV" ] = IV(X=InputData$NATIVECOUNTRY, Y=InputData$ABOVE50K)[1]

iv_df[iv_df$VARS == "AGE", "IV" ] = IV(X=as.factor(InputData$AGE), Y=InputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "EDUCATIONNUM", "IV" ] = IV(X=as.factor(InputData$EDUCATIONNUM), Y=InputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "FNLWGT", "IV" ] = IV(X=as.factor(InputData$FNLWGT), Y=InputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "HOURSPERWEEK", "IV" ] = IV(X=as.factor(InputData$HOURSPERWEEK), Y=InputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "CAPITALGAIN", "IV" ] = IV(X=as.factor(InputData$CAPITALGAIN), Y=InputData$ABOVE50K)[1]
iv_df[iv_df$VARS == "CAPITALLOSS", "IV" ] = IV(X=as.factor(InputData$CAPITALLOSS), Y=InputData$ABOVE50K)[1]

logitMod = glm(ABOVE50K ~ MARITALSTATUS + AGE + OCCUPATION + EDUCATION +
                 HOURSPERWEEK + CAPITALGAIN + SEX, data = trainingData, family = binomial(link = "logit"))

#predicted scores
Predicted = predict(logitMod, testData, type = "response")
summary(logitMod)


    plotROC(testData$ABOVE50K, Predicted)
