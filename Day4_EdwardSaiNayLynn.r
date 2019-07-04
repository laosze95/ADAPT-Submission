setwd("C:/Users/edwards/OneDrive - PSA International/ADAPT/5 days course/day4")

# library(readxl)
# library(dplyr)
# library(ggplot2)
# library(reshape2)
# library(stringr)
# library(lubridate)
path="C:/Users/edwards/OneDrive - PSA International/ADAPT/5 days course/day4"

mtc <- mtcars
# mtc$am <- as.factor(mtcars$am)
mtc$am <- factor(mtc$am,labels=c("Automatic","Manual"))
str(mtc)
attach(mtc)

# is the mean MPG of automatic cars different from manual car? 
# State the null and alternative hypothesis.
# Assume level of significance = 5% (Hint: use the 2 sided t-test)
# Welch Two Sample t-test
t.test(mpg ~ am, var.equal = F) 
# Student t-test
t.test(mpg ~ am, var.equal = F) 
# Levene Test
install.packages("car")
library(car)
# levenetetest... ()

# is the mean value of MPG > 20?
# state the null and alternative hypothesis.
# Assume level of significance = 5% (Hint: use the 1 sided t-test)

# ==================

# State the linear equation that describe relationship between MPG and AM
# mpg = y, am = x
# can have multiple x values by appending with x sign
class(am) # make sure data type is "Factor" so that r function will generate model correctly considering AM is categorical data type
model1 <- lm(mpg ~ am)
summary(model1)

# predict MPG on a car with weight = 3000 lb
model2 <- lm(mpg ~ wt)
summary(model2)


dir()
df <- read.csv("Cleaned_Data.csv")
str(df)
df[duplicated(df[,c("CNTR_N","CNTR_WGT_KG","POL","POD","LOAD_VSL_NAME","DISC_VSL_NAME")]),]
summary(df)
