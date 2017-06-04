##Question 1

data("mtcars")
library(ggplot2)
head(mtcars)
str(factor(mtcars$cyl))
q1<-lm(mpg~factor(cyl)+wt,mtcars)
summary(q1)$coef

##Question 2

q2<-lm(mpg~as.factor(cyl),mtcars)  ## Adjusted means without wt
summary(q2)$coef[3] # without wt
summary(q1)$coef[3] # holding wt constant

##Note that 11.564 > 6.071, and so holding weight constant, cylinder appears to have less of 
# an impact on mpg than if weight is disregarded.

##Question 3

q3<-lm(mpg~factor(cyl)*wt,mtcars)
anova(q1,q3,test="Chisq")
#The P-value is larger than 0.05. So, according to our criterion, 
#we would fail to reject, which suggests that the interaction terms may not be necessary.


##Question 4
q4<-lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
summary(q4)$coef

##Question 5

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

fit<-plot(y~x)
q5<-lm(y~x)
hatvalues(q5)

##Question 6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
q6<-lm(y~x)
hatvalues(q6)
dfbetas(q6)






