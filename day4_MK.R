t.test(mtcars$mpg ~ mtcars$am, var.equal = FALSE)

Regress = lm(mtcars$mpg~mtcars$am, mtcars)

Model2 = lm(mtcars$mpg~mtcars$wt, mtcars)
summary(Model2)

Model3 = lm(mtcars$mpg~mtcars$am + mtcars$wt)
summary(Model3)

Model4 = lm(mtcars$mpg)