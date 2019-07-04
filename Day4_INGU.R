str(mtcars)
sapply(mtcars, function(x) sum(is.na(x)))
rowSums(is.na(mtcars))

mtcars2 <-within(mtcars, {
  vs <- factor(vs, labels = c("V","S"))
  am <- factor(am, labels = c("automatic","manual"))
  cyl <- ordered(cyl)
  gear <- ordered(gear)
  carb <- ordered(carb)
})

mtcars$am = factor(mtcars$am, labels = c("automatic","manual"))

str(mtcars2)
attach(mtcars2)
summary(mtcars2)
quantile(mpg)
IQR(mpg)
mean(mpg)
median(mpg)

quantile(mpg,prob = c(0.15,0.25,0.35))

unique(am)
match(am, unique(am))
tabulate(match(am,unique(am)))
which.max(tabulate(match(am,unique(am))))

#mode
#Create the function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
getmode(am)


var(mpg)
sd(mpg)

## ggplot(data=mtcars, aes(x=disp, y=mpg)) + geom_point(aes(size=hp, color=wt))

pairs(mtcars)
pairs(mtcars2[,c(1,3,4,5,6,7)])
cov(mpg,wt)
cor(mpg,wt)
cor.test(mpg,wt,method="pearson")


################################3 Day_4 Start
## Welch t-test
t.test(mpg~am,var.equal=FALSE)
## stdudent's t-test
t.test(mpg~am,var.equal=TRUE)

install.packages("car")
library(car)
leveneTest(mpg~am)

##Shapiro-wilk normality test - H0 : MP flllows a normal distribution -> not rejected(if p-value > 0.05)
shapiro.test(mtcars$mpg)

qqnorm(mpg)
qqline(mpg)

model1<-lm(mpg~am)
summary(model1)

model2<-lm(mpg~wt)
summary(model2)
