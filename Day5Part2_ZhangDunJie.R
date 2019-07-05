Adult <- read.csv(file = "C:/Users/zhangdj/Desktop/DataForExcellence/Workshop_Dataset/Day5/adult.csv", header = TRUE, sep=',')
View(Adult)

table(Adult$ABOVE50K)
barplot(table(Adult$ABOVE50K),col = "lightblue")

input_ones <- Adult[which(Adult$ABOVE50K==1),]
input_zeros <- Adult[which(Adult$ABOVE50K==0),]

set.seed(100)

input_ones_training_rows <- sample(1:nrow(input_ones),0.7 * nrow(input_ones))
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7 * nrow(input_ones))

training_ones <- input_ones[input_ones_training_rows,]
training_zeros <- input_zeros[input_zeros_training_rows,]

trainingData <- rbind(training_ones,training_zeros)

nrow(trainingData)/nrow(Adult)

testing_ones <-input_ones[-input_ones_training_rows,]
testing_zeros <-input_zeros[-input_zeros_training_rows,]
testing_Data <-rbind[testing_ones,testing_zeros]
barplot(table(Adult$ABOVE50K),col = "lightblue")

factor_vars <- c("WORKCLASS", "EDUCATION", "MARITALSTATUS", "OCCUPATION", "RELATIONSHIP", "RACE", "SEx", "NATIVECOUNTRY")
continuous_vars <- c("AGE", "FNLWGT", "EDUCATIONNUM","HOURSPERWEEK", "CAPITALGAIN", "CAPITALLOSS")
iv_df <- data.frame(VARS=c(factor_vars,continuous_vars),IV=numeric(14))
View(iv_df)

library(InformationValue)
iv_df[iv_df$VARS=="WORKCLASS","IV"] <- IV(X=Adult$WORKCLASS,Y=Adult$ABOVE50K)[1]
iv_df[iv_df$VARS=="EDUCATION","IV"] <- IV(X=Adult$EDUCATION,Y=Adult$ABOVE50K)[1]
iv_df[iv_df$VARS=="MARITALSTATUS","IV"] <- IV(X=Adult$MARITALSTATUS,Y=Adult$ABOVE50K)[1]
iv_df[iv_df$VARS=="OCCUPATION","IV"] <- IV(X=Adult$OCCUPATION,Y=Adult$ABOVE50K)[1]
iv_df[iv_df$VARS=="RACE","IV"] <- IV(X=Adult$RACE,Y=Adult$ABOVE50K)[1]
iv_df[iv_df$VARS=="SEX","IV"] <- IV(X=Adult$SEX,Y=Adult$ABOVE50K)[1]
iv_df[iv_df$VARS=="NATIVECOUNTRY","IV"] <- IV(X=Adult$NATIVECOUNTRY,Y=Adult$ABOVE50K)[1]

iv_df[iv_df$VARS=="AGE","IV"] <- IV(X=as.factor(Adult$AGE),Y=Adult$ABOVE50K)[1]
iv_df[iv_df$VARS=="FNLWGT","IV"] <- IV(X=as.factor(Adult$FNLWGT),Y=Adult$ABOVE50K)[1]
iv_df[iv_df$VARS=="EDUCATIONNUM","IV"] <- IV(X=as.factor(Adult$EDUCATIONNUM),Y=Adult$ABOVE50K)[1]
iv_df[iv_df$VARS=="HOURSPERWEEK","IV"] <- IV(X=as.factor(Adult$HOURSPERWEEK),Y=Adult$ABOVE50K)[1]
iv_df[iv_df$VARS=="CAPITALGAIN","IV"] <- IV(X=as.factor(Adult$CAPITALGAIN),Y=Adult$ABOVE50K)[1]
iv_df[iv_df$VARS=="CAPITALLOSS","IV"] <- IV(X=as.factor(Adult$CAPITALLOSS),Y=Adult$ABOVE50K)[1]

logitMod <- glm(ABOVE50K ~ MARITALSTATUS + AGE + OCCUPATION + EDUCATION + HOURSPERWEEK + CAPITALGAIN + SEX, data=trainingData, family = binomial(link="logit"))
Predicted <- predict(logitMod, testData, type="response")
summary(logitMod)

plotROC(testingData$ABOVE50K,Predicted)

colnames(cm) <- c("Actual 0", "Actual 1")
rownames(cm) <- c("Predicted 0", "Predicted 1")
fourfoldplot(as.matrix(cm))