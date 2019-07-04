#is the mean MPG of automatic cars different from manual car? State the null and alternative hypothesis.
#assume level of significance =5% (use the 2 sided t test)

#H0:mean MPG of auto car same with manual car
#Ha:mean MPG of auto car different from manual car 

#use welch two sample t-test
t.test(mpg ~ am, var.equal = FALSE)
#p-value = 0.001374
#p value <0.05, H0 is rejected. there is significant evidence mean mpg between manual and auto not equal

#notes:
#test of normality
##can use: qqnorm(mpg) or qqline(mpg)
##also can use shapiro.test(mtcars$mpg)

#notes:
#test of equal variances
##use levene test
#can use then levenetest(mpg~am)

#is the mean value of MPG > 20? State the null and alternative hypothesis. Assume level of signficance = 5%
#use the 1 sided t test 

#H0:mean MPG of > 20
#Ha:mean MPG of ??? 20
    
##state the linear equation that describes the relationship between MPG and type of transmission

simple.fit = lm(mtcars$mpg ~ mtcars$am, data=mtcars)
summary(simple.fit)
##first one always y axis, the one you want to predict, which in this case is mtcars$mpg

##predict the MPG of a car with weight = 3000lb
model2<- lm(mpg~wt) ##if want to include all variables, replace wt with a .
summary(model2)

#model:
#mpg1 = 37.2851 - 5.3445 wt1