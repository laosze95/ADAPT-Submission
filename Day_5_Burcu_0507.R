
ADAPT_Training = "/csvfolder"
list.files(path="C:\Users\btutar\Desktop\ADAPT-master", pattern="*.csv", full.names=TRUE)


path ="C:\Users\btutar\Desktop\ADAPT-master"
filenames=dir(path)

# getting csv

Modeldata = read.csv('C:/Users/btutar/Desktop/ADAPT-master/ModelData_Quarterly.csv')

# deleting last columns
Modeldata=Modeldata[,1:6]

# arima needs those packages
library(InformationValue)
library(tseries)
library(forecast)

#  firt inceleme
str(Modeldata)
dim(Modeldata)
summary(Modeldata)
View(summary(Modeldata))
NAs_List <- colSums(is.na(Modeldata)); NAs_List[NAs_List > 0]
View(sapply(Modeldata, function(x) sum(is.na(x))))

# Time series with ggplot for the 4th column by writing column number
plot(ts(Modeldata[,5]))
ts(Modeldata[,5])
class(ts(Modeldata[,5]))

# Time series with ggplot for the 4th column by writing column name
plot(ts(Modeldata$Throughput))

# check if the mean & variance are stationary
var(Modeldata$Throughput)
mean(Modeldata$Throughput)
sd(Modeldata$Throughput)
plot(diff(ts((Modeldata$Throughput))))

#for writing titles
plot(diff(ts((Modeldata$Throughput))), main='burcus',xlab='data Points')

Arimafit = auto.arima(Modeldata$Throughput, approximation=FALSE,trace=TRUE, seasonal=TRUE)

#Create the table to compare actual vs predicted (error:Difference btw actual and predicted)

Table1 <- data.frame("Actual"= as.numeric(Modeldata$Throughput),"Period"= Modeldata$Year.and.Quarter,
                     Prediction=as.numeric(Arimafit$fitted),Error=round(as.numeric(Arimafit$residuals), digits=2))

#forecast the next four quarter using ARIMA Model with 95% confidence level

Tablex<-data.frame(forecast(Arimafit,h=4,level=95))
Tablex$Periodx <- c('2019-Q1','2019-Q2','2019-Q3','2019-Q4')

# plot the historical and forecasted value (since q1 2000)

plot(forecast(Arimafit, h=4, level=95))

# trying to combine 2 tables

View(Table1)
str(Table1)
Table1$Period = as.character(Table1$Period)
Tablex

My_Table <- rbind(Table1$Actual,Table1$Period,Table1$Prediction,Table1$Error)

Tablex[nrow(Table1)+1,] <- list("2019-Q1",NA,198.4892,NA)
Table1[nrow(Table1)+2,] <- list("2019-Q2",NA,203.9078,NA)
Table1[nrow(Table1)+1,] <- list("2019-Q3",NA,204.7478,NA)
Table1[nrow(Table1)+1,] <- list("2019-Q4",NA,203.3269,NA)
Table1
Table1[77,] <- list("2019-Q1",NA,198.4892,NA)
write.csv(Table1, "D:/timeseries.csv")


     