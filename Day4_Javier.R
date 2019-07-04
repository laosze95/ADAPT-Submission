#Javier Kan Day 4 Exercise
library(dplyr)
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(forecast)
library(GGally)
library(PerformanceAnalytics)
library(psych)
library(reshape2)
library(stringr)
library(tseries)
library(InformationValue)
library(data.table)

#Understanding the structure of mtcars
str(mtcars)
dim(mtcars)
colSums(is.na(mtcars))
summary(mtcars)

#Change to factor
mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V","S"))
  am <- factor(am, labels = c("automatic","Manual"))
  cyl <- ordered(cyl)
  gear <- ordered(gear)
  carb <- ordered(carb)
})
mtcars2

#Mode
#Create function
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
getmode(mtcars2$am)

#Variance
var(mpg)
#Standard deviation
sd(mpg)

#Density chart
attach(mtcars2)
plot(density(mpg))

#Covariance and correlation
cov(mtcars$cyl,mtcars$mpg)
cor(mtcars$cyl,mtcars$mpg)

#Test of correlation
pairs(mtcars)
cor.test(mpg,wt,method="pearson")

#Mean mpg btw manual and automatic cars are equal (Welch)
t.test(mpg ~ am, var.equal=FALSE)
#Student t-test
t.test(mpg ~ am, var.equal=TRUE)

#Test of normality
shapiro.test(mtcars$mpg)
qqnorm(mpg)
qqline(mpg)

#Linear regression mpg and transmission
model1 <- lm(mpg ~ am)
summary(model1)
#Linear regression mpg and weight
model2 <- lm(mpg ~ wt)
summary(model2)
predict(model2)

