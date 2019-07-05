df_Arima=read.csv('C:/Users/maloise/Desktop/ADAPT/Day 5/ModelData_Quarterly.csv')

str(df_Arima)

#To delete the last columns
df_Arima=df_Arima[,1:6]

# view columns
View(df_Arima)

install.packages("InformationValue")

# Load next libraries 
library(InformationValue)

library(tseries)

library(forecast)

View(df_Arima)
summary(df_Arima)
class(df_Arima)
ncol(df_Arima)
nrow(df_Arima)

plot(df_Arima$Throughput)

# To change to other type of line
plot(df_Arima$Throughput,type = 'l')

plot(ts(df_Arima[,4]))

# Shows all points of the plot
ts(df_Arima[,4])

plot(diff(ts(df_Arima[,4])))

plot(diff(ts(df_Arima[,4])),main='Differenced Container Throughput Time Series', xlab='Data points')

# Build the auto ARIMA model
auto.arima(df_Arima$Throughput)

# Create a new table
df_AVP=as.data.frame(cbind(as.character(df_Arima$Year.and.Quarter),df_Arima$Throughput,arimafit$fitted,arimafit$residuals))
# Read the columns
colnames(df_AVP)
# Rename the columns
colnames(df_AVP)=c("Period","Actual","Prediction","Error")

# Forecast the next four quartes using ARIMA model along with 95% confidence level
forecast(arimafit)

# If we need only 4 rows
forecast(arimafit, h=4)

# If we need only 4 rows and no Lo 80 & Hi 80 columns
forecast(arimafit, h=4, level=95)

# to plot the previous result
plot(forecast(arimafit, h=4, level=95))

as.numeric(df_AVP$Actual)

df_AVP$Actual=as.numeric(df_AVP$Actual)
df_AVP$Prediction=as.numeric(df_AVP$Prediction)
df_AVP$Error=as.numeric(df_AVP$Error)

df_AVP[77,] <- list("2019-Q1", NA, 198.4892, NA)
view(df_AVP)
df_AVP[78,] <- list("2019-Q2", NA, 203.9078, NA)
df_AVP[79,] <- list("2019-Q3", NA, 204.7478, NA)
df_AVP[80,] <- list("2019-Q4", NA, 203.3269, NA)

# Load "adult" file
Adult=read.csv('C:/Users/maloise/Desktop/ADAPT/Day 5/adult.csv')

# Exploring data
View(Adult)
nrow(Adult)
ncol(Adult)
View(Adult)
summary(Adult)
class(Adult)
is.na(Adult)
sum(is.na(Adult))
typeof(Adult)
str(Adult)

# Chequing BIAS of data
table(Adult$ABOVE50K)
barplot(table(Adult$ABOVE50K),col="orange")


input_ones <- Adult[which(Adult$ABOVE50K == 1), ]
input_zeros <- Adult[which(Adult$ABOVE50K == 0), ]

set.seed(100)

input_one_trainig_rows <- sample(1:nrow(input_ones), 0.7* nrow(input_ones))

input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_zeros))

training_ones <- input_ones[input_one_trainig_rows, ]
training_zeros <- input_zeros[input_zeros_training_rows, ]

trainigData <- rbind(training_ones, training_zeros)
barplot(table(trainigData$ABOVE50K),col="orange")

test_ones <- input_ones[-input_one_trainig_rows, ]
test_zeros <- input_zeros[-input_one_trainig_rows, ]

testData <- rbind(test_ones, test_zeros)

barplot(table(testData$ABOVE50K),col="orange")

# Segregate continous and factor variables
factor_vars <- c ("WORKCLASS", "EDUCATION", "MARITALSTATUS", "OCCUPATION", "RELATIONSHIP", "RACE", "SEX", "NATIVECOUNTRY")
continuous_vars <- c("AGE", "FNLWGT", "EDUCATIONNUM", "HOURSPERWEEK", "CAPITALGAIN", "CAPITALLOSS")

# Initialization
iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(14))
 
# Compute IV for categorical variables
iv_df[iv_df$VARS == "WORKCLASS", "IV"] <- IV(X=inputData$WORKCLASS, Y=inputData$ABOVE50K) [1]
iv_df[iv_df$VARS == "EDUCATION", "IV"] <- IV(X=inputData$EDUCATION, Y=inputData$ABOVE50K) [1]
iv_df[iv_df$VARS == "MARITALSTATUS", "IV"] <- IV(X=inputData$MARITALSTATUS, Y=inputData$ABOVE50K) [1]
iv_df[iv_df$VARS == "OCCUPATION", "IV"] <- IV(X=inputData$OCCUPATION, Y=inputData$ABOVE50K) [1]
iv_df[iv_df$VARS == "RACE", "IV"] <- IV(X=inputData$RACE, Y=inputData$ABOVE50K) [1]
iv_df[iv_df$VARS == "SEX", "IV"] <- IV(X=inputData$SEX, Y=inputData$ABOVE50K) [1]
iv_df[iv_df$VARS == "NATIVECOUNTRY", "IV"] <- IV(X=inputData$NATIVECOUNTRY, Y=inputData$ABOVE50K) [1]
iv_df[iv_df$VARS == "AGE", "IV"] <- IV(X=inputData$AGE, Y=inputData$ABOVE50K) [1]
iv_df[iv_df$VARS == "EDUCATIONNUM", "IV"] <- IV(X=inputData$EDUCATIONNUM, Y=inputData$ABOVE50K) [1]
iv_df[iv_df$VARS == "RELATIONSHIP", "IV"] <- IV(X=inputData$RELATIONSHIP, Y=inputData$ABOVE50K) [1]
iv_df[iv_df$VARS == "FNLWG", "IV"] <- IV(X=inputData$FNLWGT, Y=inputData$ABOVE50K) [1]
iv_df[iv_df$VARS == "HOURPERWEEK", "IV"] <- IV(X=inputData$HOURSPERWEEK, Y=inputData$ABOVE50K) [1]
iv_df[iv_df$VARS == "CAPITALGAIN", "IV"] <- IV(X=inputData$CAPITALGAIN, Y=inputData$ABOVE50K) [1]
iv_df[iv_df$VARS == "CAPITALLOSS", "IV"] <- IV(X=inputData$CAPITALLOSS, Y=inputData$ABOVE50K) [1]

iv_df <- iv_df[order(-iv_df$IV),]
iv_df

logitMod <- glm(ABOVE50K ~ MARITALSTATUS + AGE + OCCUPATION + EDUCATION + HOURSPERWEEK + CAPITALGAIN + SEX, data=trainigData, family = binomial(link = "logit"))


predicted <- predict(logitMod, testData, type="response")
summary(logitMod)

# Testing ans validation
plotROC(testData$ABOVE50K, predicted)


colnames(cm) <- c("Actual 0", "Actual 1")
rownames (cm) <- c("Predicted 0", "Predicted 1")
fourfoldplot(as.matrix(cm))
