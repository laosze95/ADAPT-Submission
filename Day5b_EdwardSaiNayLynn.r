setwd("C:/Users/edwards/OneDrive - PSA International/ADAPT/5 days course/day5")

library(readxl)
library(dplyr)
library(ggplot2)
# library(reshape2)
# library(stringr)
# library(lubridate)
path="C:/Users/edwards/OneDrive - PSA International/ADAPT/5 days course/day5"

# Adult data set
dir()
df <- read.csv("adult.csv")
dim(df)
str(df)
summary(df)
# convert to tbl_df data type which has same data frame feature but easier to browse around
df <- tbl_df(df)

# check how many records are for above 50k and how many otherwise
table(df$ABOVE50K)
barplot(table(df$ABOVE50K), col = "lightblue")

df_ones <- df[which(df$ABOVE50K ==1), ] # all 1's
df_zeros <- df[which(df$ABOVE50K == 0), ] # all 0's

set.seed(100) # for repeatability of samples
# we will take 70% of the lower category (i.e. Above$50) for training data
df_ones_trng_rows <- sample(1:nrow(df_ones), 0.7*nrow(df_ones))
df_zeros_trng_rows <- sample(1:nrow(df_ones), 0.7*nrow(df_ones))

length(df_ones_trng_rows)
length(df_zeros_trng_rows)

# Get the training data
trng_ones <- df_ones[df_ones_trng_rows,]
trng_zeros <- df_zeros[df_zeros_trng_rows,]
dim(trng_ones)
dim(trng_zeros)
trng <- rbind(trng_ones,trng_zeros)
dim(trng)
table(trng$ABOVE50K)

# Get the test data
test_ones <- df_ones[-df_ones_trng_rows,]
test_zeros <- df_zeros[-df_zeros_trng_rows,]
test <- rbind(test_ones,test_zeros)
dim(test)

# check duplicate count in training data and test data
length(which(duplicated(c(rownames(test),rownames(trng)))))

# select the significant features affecting the income level
# aka Feature Engineering: to find out the most significant factors affecting the outcome

# segregate continuous and factor variables
factor_vars <- c("WORKCLASS","EDUCATION","MARITALSTATUS","OCCUPATION","RELATIONSHIP","RACE","SEX","NATIVECOUNTRY")
continuous_vars <- c("AGE","FNLWGT","EDUCATIONNUM","HOURSPERWEEK","CAPITALGAIN","CAPITALLOSS")

# initialization for the IV results
library(InformationValue)
iv_df <- data.frame(VARS=c(factor_vars,continuous_vars),IV=numeric(14))

iv_df[iv_df$VARS == "WORKCLASS", "IV"] <- IV(X=df$WORKCLASS,Y=df$ABOVE50K)[1]
iv_df[iv_df$VARS == "EDUCATION", "IV"] <- IV(X=df$EDUCATION,Y=df$ABOVE50K)[1]
iv_df[iv_df$VARS == "MARITALSTATUS", "IV"] <- IV(X=df$MARITALSTATUS,Y=df$ABOVE50K)[1]
iv_df[iv_df$VARS == "OCCUPATION", "IV"] <- IV(X=df$OCCUPATION,Y=df$ABOVE50K)[1]
iv_df[iv_df$VARS == "RELATIONSHIP", "IV"] <- IV(X=df$RELATIONSHIP,Y=df$ABOVE50K)[1]
iv_df[iv_df$VARS == "RACE", "IV"] <- IV(X=df$RACE,Y=df$ABOVE50K)[1]
iv_df[iv_df$VARS == "SEX", "IV"] <- IV(X=df$SEX,Y=df$ABOVE50K)[1]
iv_df[iv_df$VARS == "NATIVECOUNTRY", "IV"] <- IV(X=df$NATIVECOUNTRY,Y=df$ABOVE50K)[1]

iv_df[iv_df$VARS == "AGE", "IV"] <- IV(X=as.factor(df$AGE),Y=df$ABOVE50K)[1]
iv_df[iv_df$VARS == "FNLWGT", "IV"] <- IV(X=as.factor(df$FNLWGT),Y=df$ABOVE50K)[1]
iv_df[iv_df$VARS == "EDUCATIONNUM", "IV"] <- IV(X=as.factor(df$EDUCATIONNUM),Y=df$ABOVE50K)[1]
iv_df[iv_df$VARS == "HOURSPERWEEK", "IV"] <- IV(X=as.factor(df$HOURSPERWEEK),Y=df$ABOVE50K)[1]
iv_df[iv_df$VARS == "CAPITALGAIN", "IV"] <- IV(X=as.factor(df$CAPITALGAIN),Y=df$ABOVE50K)[1]
iv_df[iv_df$VARS == "CAPITALLOSS", "IV"] <- IV(X=as.factor(df$CAPITALLOSS),Y=df$ABOVE50K)[1]

# View the IV result and sort output by IV column in descending order
View(iv_df)

# take the top 5 fields with high IV value to build model using training dataset
logitMod <- glm(ABOVE50K ~ RELATIONSHIP + MARITALSTATUS + AGE + OCCUPATION + EDUCATION + EDUCATIONNUM + HOURSPERWEEK + CAPITALGAIN + SEX, data=trng, family = binomial(link="logit"))

# predicted scores using test data
predicted <- predict(logitMod, test, type="response")
summary(logitMod)

# Checking prediction performance by comparing actual data vs predicted data
plotROC(test$ABOVE50K, predicted)


# plot Confusion Matrix
cm <- confusionMatrix(test$ABOVE50K, predicted)
colnames(cm) <- c("Actual 0", "Actual 1")
rownames(cm) <- c("Predicted 0", "Predicted 1")
fourfoldplot(as.matrix(cm))
