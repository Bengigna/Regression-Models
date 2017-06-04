#Q1 
# Least square sum (i=1 to n) =wi(xi-u)2)

x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)

minu <- sum(x*w) / sum(w)
final <- sum(w*(x-minu)^2)
c(minu, final)

# The given means from the question

mu <- c(0.1471, 1.077, 0.0025, 0.300)

#Let us see what result we get from each mu using the equation given
for (i in mu)
{print( c(i, sum(w*(x-i)^2)) )}

#Q2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
fit2<-lm(y~x)
summary(fit2)
#or 
sum(y * x)/sum(x^2)
#or
coef(lm(y ~ x - 1))

#Q3
data(mtcars)
fit3<-lm(mtcars$mpg~mtcars$wt)
summary(fit3)

#or 
x <- mtcars$wt
y <- mtcars$mpg
cor(x,y) *sd(y)/sd(x)

#Q6

x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
zx <- (x-mean(x)) / sd(x)
zx[1]

#Q7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
fit7<-lm(y~x)
summary(fit7)

#or
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
plot(y,x)
b1 <- cor(x,y)*sd(y)/sd(x)
b0 <- mean(y) - b1 * mean(x)

#Q8

b1=1000000000000
b0 <- 0 - b1 *0

#Q9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)

for(u in c(0.573, 0.8, 0.36, 0.44))
{
  SSE <- sum((x-u)^2)
  print(c(u, SSE))
}

#Q10
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
fit11<-lm(y~x)
fit12<-lm(x~y)
summary(fit11)
summary(fit12)
slope1<-cor(y,x)*sd(y)/sd(x)
slope2<-cor(x,y)*sd(x)/sd(y)
slope1/slope2
var(y)/var(x)

