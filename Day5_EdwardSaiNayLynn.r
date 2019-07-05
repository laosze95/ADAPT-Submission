setwd("C:/Users/edwards/OneDrive - PSA International/ADAPT/5 days course/day5")

library(readxl)
library(dplyr)
library(ggplot2)
# library(reshape2)
# library(stringr)
# library(lubridate)
path="C:/Users/edwards/OneDrive - PSA International/ADAPT/5 days course/day5"

df <- read.csv("ModelData_Quarterly.csv")
df <- select(df,-(X:X.2))

str(df)
summary(df)

# plotting the trending
plot(df$Year.and.Quarter, df$Throughput)

# plotting the distribution in histogram chart
hist(df$Throughput)

# boxplot graph
boxplot(df$Throughput)

# density population of throughput
plot(density.default(df$Throughput))


# quarterly distribution
plot(df$Quarter, df$Throughput)
# yearly distribution
plot(df$Year, df$Throughput)

plot(df %>% group_by(Year) %>% summarise(ytd = sum(Throughput)))

#Using GGPLOT
# yearly trend
df %>% group_by(Year) %>% summarise(ytd = sum(Throughput)) %>% ggplot(aes(Year, ytd)) + geom_path() 
# quarterly bar
df %>% group_by(Quarter) %>% summarise(avg = mean(Throughput)) %>% ggplot(aes(Quarter, avg)) + geom_col() 


# libraries for time series analysis
install.packages("tseries")
install.packages("forecast")
library(tseries)
library(forecast)


# plot using Time Series
plot(ts(df[,4]))
ts(df[,4])
nrow(df)

# check if the Mean and Variance are stationary
plot(ts(diff(df$Throughput)))
# the mean of variances should be closed to zero
mean(ts(diff(df$Throughput)))


# Build the Auto ARIMA Model
am <- auto.arima(df[,4])
class(am)
am2 <- auto.arima(df[,4], approximation = F, trace = T, seasonal = T)

m <- arima(df[,4])
class(m)


dm <- data.frame(df$Year.and.Quarter,df$Throughput)
dm <- cbind(dm,as.data.frame(as.numeric(am$fitted)))
dm <- rename(dm, "Period" = "df.Year.and.Quarter",
             "Actual" = "df.Throughput",
             "Prediction" = "as.numeric(am$fitted)")
dm$Error <- dm$Actual-dm$Prediction
str(dm)
summary(dm)

# plot the prediction & actual
plot(ts(dm$Prediction))
plot(ts(dm$Actual))
ggplot(dm,aes(Period)) + 
  geom_line(aes(y=Actual,group=1,color="green")) + 
  geom_line(aes(y=Prediction,group=1,color="red")) +
  theme(legend.position="top")
# note: colors not correctly drawn

# error chart.. it shows that prediction differs a lot during 2008 
# because time-series can't predict for such sudden changes
plot(ts(dm$Error))

# Forecase the next 4 quarters with lo/hi 95%
fc <- forecast(am,h=4,level = 95)
class(fc)
fc
plot(fc)


# Combine historical and Forecast into a file
dm
fcdf <- data.frame(
  c("2019-Q1","2019-Q2","2019-Q3","2019-Q4"),
  rep(NA,4),
  fc$mean,
  rep(NA,4)
)
colnames(fcdf) <- c("Period","Actual","Prediction","Error")
fcdf
dm2 <- rbind(dm,fcdf)
dm2
str(dm2)
summary(dm2)

# save the forcast data to file
write.csv(dm2,paste(path,"Day5_TimeSeries_EdwardSai.csv",sep = "/"))
dir()

