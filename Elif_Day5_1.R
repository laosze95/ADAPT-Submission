#veriyi almak istediðin yeri sabitle

setwd("C:/Users/edursun/Desktop/ADAPT/ADAPT_4")
df_Data <- read.csv(file="C:/Users/edursun/Desktop/ADAPT/ADAPT_4/ModelData_Quarterly.csv",
                    header=TRUE, sep=",")

# veride son satýrlarda boþta olan sütunlarý silmek için 
df_Data_new=df_Data[,-7:-9]
df_Data_new


# ARIMA kullabilmek içn bunlarý çaðýrdýk
library(InformationValue)
library(tseries)
library(forecast)

# verinin özelliklerine bakmak için bunu yapýyoruz

dim(df_Data_new)
str(df_Data_new)
sapply(df_Data_new, class)
summary(df_Data_new)
sapply(df_Data_new, function(x) sum(length(which(is.na(x))))) 

library(ggplot2)

ggplot(df_Data_new, aes(df_Data_new$Throughput)) + geom_line()


#ts kullanarak time series çizgisi yapýyoruz
plot(ts(df_Data_new[,4]))


#ggplot deneme için

ggplot(df_Data_new, aes(x=df_Data_new$Year.and.Quarter, y=df_Data_new$Throughput))
 
?scale_x_date


#check the mean & variance are stationary
plot(diff(ts(df_Data_new[,4])))

#auto arima 
model=auto.arima(df_Data_new$Throughput,approximation = FALSE, trace=TRUE,seasonal=TRUE)
model

#auto arýma dan gelen fitted prediction, residuals error seçilir

fitted=model$fitted
fitted
residuals=model$residuals

#prediction ve error adý altýnda yeni sutunlar açýyoruz yeni bir tablo yaratmýþ oluyoruz

df_Data_new$prediction=model$fitted
df_Data_new$error=model$residuals


plot(ts(df_Data_new$prediction))

#forecasting for the next values "77-80" row


frc=forecast(model,4,95)
frc$fitted[77:80]
plot(frc)
?rbind

str(df_Data_new)
df_Data_new$Year.and.Quarter=as.character(df_Data_new$Year.and.Quarter)
df_Data_new$prediction=as.numeric(df_Data_new$prediction)
df_Data_new$error=as.numeric(df_Data_new$error)


df_Data_new=df_Data_new[,-5:-6]

frc
df_Data_new[77,] <- list(NA,NA,"2019-Q1",NA,198.4892,NA,NA,NA)
df_Data_new[78,] <- list("2019-Q2",NA,198.4892,NA)

