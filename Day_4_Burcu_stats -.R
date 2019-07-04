
# stats analysis 

attach(mtcars)

# which car is View(sapply(df_Data, function(x) sum(is.na(x))))
# how many data itesm are there, rows, numerical,categorical,missing data

dim(mtcars)
str(mtcars)
View(sapply(mtcars, function(x) sum(is.na(x))))
summary(mtcars$mpg)
mode(mtcars$Transmission)

#convert column to am

mtcars$am=factor(mtcars$am, labels=c("Automatic","manual"))

View(sapply(mtcars, function(x) sum(is.na(x))))
str(mtcars)
attach(mtcars)
rowSums(is.na(mtcars))
summary(mtcars$mpg)

getmode <- function (v) {
  uniqv <- unique (v)
  uniqv[which.max(tabulate(match(v, unique)))]
}
# breakdown of auto and manual
mtcars %>% ggplot(aes(am)) + geom_bar()

# distirbution of MPG

ggplot(mtcars, aes(mpg)) + geom_histogram(binwidth=5)

# variance of mpg
var(mpg)

# std  of mpg
sd(mpg)
density(x=mpg)

# correlation and cov

cor(mtcars$cyl,mtcars$disp)
cor(mtcars$cyl,mtcars$drat)
cor(mtcars$cyl,mtcars$wt)   
cor(mtcars$cyl,mtcars$qsec)
cor(mtcars$cyl,mtcars$gear)
cor(mtcars$cyl,mtcars$carb)

pairs(mtcars)
pairs(mtcars)

cov(mpg,wt)
cor(mpg,wt)

#mean MPG of auto and manual are different, null and alternative tests sig.level=5%
library(car)
levene.test(mpg~am)
t.test(mpg ~ am, var.equal=TRUE)
shapiro.test(mtcars$mpg)

#linear equation that describes the relationship between mpg
#and type of transmission
model1 <- lm(mpg~am)
summary(model1)
model2<- lm(mpg ~ wt)
summary(model2)
# predict(model2) 
