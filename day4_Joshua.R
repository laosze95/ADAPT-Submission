# do a t-test to compare mean of automatic vs manual
t.test(mtcars$mpg ~ mtcars$am, var.equal=FALSE)
t.test(mtcars$mpg ~ mtcars$am, var.equal=TRUE)

#test if it is a normal distribution
shapiro.test(mtcars$mpg)

#linear equation for mpg and transmission
lm(mtcars$mpg~mtcars$am)

#linear equation for mpg and weight
lm(mtcars$mpg~mtcars$wt)

#estimate car with weight 3000lb, mpg is 21.253 

