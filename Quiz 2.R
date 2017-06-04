##Question 1
library(ggplot2)
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

fit<-lm(y~x)
coefTable <- coef(summary(fit))
(pval <- coefTable[2, 4])

### second method 
n <- length(y)
beta1 <- cor(y, x) * sd(y) / sd(x) ##Slope
beta0 <- mean(y) - beta1 * mean(x) ## Intercept
e <- y - beta0 - beta1 * x         ## Residual;y=response 
sigma <- sqrt(sum(e ^ 2) / (n - 2)) ## residual standard deviation around the regression line
ssx <- sum((x - mean(x)) ^ 2)   ##sum of square Xs
seBeta0<-(1/n+mean(x)^2/ssx)^0.5*sigma
seBeta1 <- sigma / sqrt(ssx)
tBeta1 <- beta1 / seBeta1
(pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE))

##Question 2
sigma <- sqrt(sum(e ^ 2) / (n - 2))
summary(fit)$sigma

##Question 3

###first method

data(mtcars)
y <- mtcars$mpg
x <- mtcars$wt
fit_car <- lm(y ~ x)
predict(fit_car, newdata = data.frame(x = mean(x)), interval = ("confidence"))

###second method
yhat <- fit_car$coef[1] + fit_car$coef[2] * mean(x)
yhat + c(-1, 1) * qt(.975, df = fit_car$df) * summary(fit_car)$sigma / sqrt(length(y))

###third method

data("mtcars")
fit3<-lm(mpg~wt,mtcars)
coef3<-summary(fit3)$coefficient
Ympg<-coef3[1,1]+coef3[2,1]*mean(mtcars$wt)
YY<-Ympg+c(-1,1)*qt(0.975,df=fit$df)*coef3[2,2]
summary(YY)

###fourth method
data(mtcars)
fit <- lm(mpg ~ I(wt - mean(wt)), data = mtcars)
confint(fit)

##Question 5
###first method
predict(fit_car, newdata = data.frame(x = 3), interval = ("prediction"))

###second method

yhat_2 <- fit_car$coef[1] + fit_car$coef[2] * 3
yhat_2 + c(-1, 1) * qt(.975, df = fit_car$df) * summary(fit_car)$sigma *
  sqrt(1 + (1/length(y)) + ((3 - mean(x)) ^ 2 / sum((x - mean(x)) ^ 2)))

##Question 6

fit_car2<-lm(y ~ I(x/2))
sumCoef2 <- summary(fit_car2)$coefficient ##or coef(summary(fit_car2))
(sumCoef2[2,1] + c(-1, 1) * qt(.975, df = fit_car2$df) * sumCoef2[2, 2])

##or
fit <- lm(mpg ~ I(wt * 0.5), data = mtcars)
confint(fit)[2, ]

##Question 7
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
fit$coef[2]

x_meter <- x / 100
fit_meter <- lm(y ~ x_meter)
fit_meter$coef[2]

##Question 8

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
fit$coef

x_a<-x+5
fit1 <- lm(y ~ x_a)
fit1$coef

##the slope does not change, but so did the intercept

fit$coefficients[1]-5*fit$coefficients[2]

##Question 9
data(mtcars)
y <- mtcars$mpg
x <- mtcars$wt
fit_car <- lm(y ~ x)
fit22<-lm(y ~ x-1) ## without intercept

ssy <- sum((y - mean(y)) ^ 2)  ## model with only intercept
sum_b0b1<-sum(resid(fit_car)^2) ##model with slope and intercept
sum_b0<-sum(resid(fit22)^2)

sum_b0b1/ssy
sum_b0b1/sum_b0

##or
fit1 <- lm(mpg ~ wt, data = mtcars)
fit2 <- lm(mpg ~ 1, data = mtcars)
1 - summary(fit1)$r.squared


##Question 10

data(mtcars)
y <- mtcars$mpg
x <- mtcars$wt
fit_car <- lm(y ~ x)
sum(resid(fit_car))

## if intercept not included
fit_car_noic <- lm(y ~ x - 1)
sum(resid(fit_car_noic))

##if intercept included (ic)
fit_car_ic <- lm(y ~ rep(1, length(y)))
sum(resid(fit_car_ic))

