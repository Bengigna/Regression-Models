##Q1
library(MASS)
data(shuttle)
str(shuttle)

shuttle$usebin <- as.numeric(shuttle$use == "auto") # create a binary variable
fit <- glm(usebin ~ factor(wind)-1, family = "binomial", data = shuttle)
Coef <- coef(summary(fit))
coef.odds <- exp(c(Coef[1, 1], Coef[2, 1])) # propability of the head and tail
(odds.ratio <- coef.odds[1] / coef.odds[2]) # "head" is the reference

## Other way
library(MASS)
data(shuttle)
## Make our own variables just for illustration
shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind, data = shuttle, family = binomial)

## other way
fit <- glm(relevel(use, "noauto") ~ relevel(wind, "tail"), data = shuttle, family = binomial)
exp(coef(fit))
exp(coef(fit))

##Q2

shuttle$usebin <- as.numeric(shuttle$use == "auto") # create a binary variable
fit2 <- glm(usebin ~ factor(wind)+factor(magn)-1, family = "binomial", data = shuttle)
Coef2 <- coef(summary(fit2))
coef2.odds <- exp(c(Coef2[1, 1], Coef2[2, 1]))
(odds2.ratio <- coef2.odds[1] / coef2.odds[2])


##Q3
shuttle$usebin <- as.numeric(shuttle$use == "auto") # create a binary variable
fit3 <- glm((1-usebin) ~ factor(wind)-1, family = "binomial", data = shuttle)
coef(summary(fit3))

##Q4
data("InsectSprays")
str(InsectSprays)
fit4<-glm(count~factor(spray),family="poisson", InsectSprays)
coef4<-coef(summary(fit4))
exp(coef4[1,1])/exp(coef4[2,1]+coef4[1,1])## expontiate of intercept-gives odds

##Other way
fit <- glm(count ~ relevel(spray, "B"), data = InsectSprays, family = poisson)
exp(coef(fit))[2]

##Q5
fit5 <- glm(count ~ factor(spray) + offset(log(rep(sum(count), length(count)))), 
            family = "poisson", data = InsectSprays)
fit5_10 <- glm(count ~ factor(spray) + 
                       offset(log(10) + log(rep(sum(count), length(count)))), 
               family = "poisson", data = InsectSprays)
coef(summary(fit5))
coef(summary(fit5_10))

##Q6
install.packages("ggplot2")
library(ggplot2)
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
z<-plot(y~x)
fitt<-lm(y~x)
abline(fitt)
knots<-0
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xMat <- cbind(1, x, splineTerms)
fit6 <- lm(y ~ xMat - 1)
summary(fit6)

yhat <- predict(fit6)
plot(x, y, frame = FALSE, pch = 21, bg = "green", cex = 2)
lines(x, yhat, col = "red", lwd = 2)
fit6$coef[2] + fit6$coef[3]


## Other method
z <- (x > 0) * x
fit <- lm(y ~ x + z)
sum(coef(fit))[2:3]
