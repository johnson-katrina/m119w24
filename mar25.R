rm(list=ls())
library(data4soils)
Ng <- cfbp_fpjuliet$ng

mean(Ng)
var(Ng)


#Method of Moments fitted function f1 (code copied from Mar 22 wiki).
f1 <- function(x,m=0,s=1){1/(sqrt(2*pi*s^2))*exp(-0.5*((x-m)/s)^2)}

mu <- mean(Ng)
sigma <- sqrt(var(Ng))
#sigma <- sd(Ng)

x1 <- seq(-20,20,0.1)
y1 <- f1(x1,m=mu,s=sigma)

par(mfrow=c(1,1),mar=c(2.5,2.5,1,0.25))
hist(Ng, probability = TRUE, main="Fitted Normal",xlim=c(-5,12))
lines(x1,y1,col=2)



#Method of Moments fitted function f2 (code copied from Mar 22 wiki).
f2 <- function(x,a=1,b=1){b^a/gamma(a)*(x)^(a-1)*exp(-b*x)}

alpha <- mean(Ng)^2/var(Ng)
alpha
beta <- mean(Ng)/var(Ng)
beta

x2 <- seq(0,20,0.1)
y2 <- f2(x2,a=alpha,b=beta)

par(mfrow=c(1,1),mar=c(2.5,2.5,1,0.25))
hist(Ng, probability = TRUE, main="Fitted Gamma")
lines(x2,y2,col=2)



#Graphing fitted f1 and f2 on the same plot.
par(mfrow=c(1,1),mar=c(2.5,2.5,1,0.25))
hist(Ng,probability = TRUE)
lines(x1,f1(x1,mu,sigma),col=2)
lines(x2,f2(x2,alpha,beta),col='blue')


#Use Fitted f2 to create a similation to approximate P(X>10).
alpha <- mean(Ng)^2/var(Ng)
beta <- mean(Ng)/var(Ng)

set.seed(123)
tmp <- rgamma(25000, shape = alpha, rate = beta)

x <- length(which(tmp > 10))
p <- x/length(tmp)
p
#0.04416
    #This is pretty close to the value 
    #we get from integrating, 0.0429819.

#Use Fitted f1 to create a similation to approximate P(X>10).
m = mean(Ng)
s = sqrt(var(Ng))

set.seed(123)
tmp <- rnorm(25000, mean=m, sd=s)

x <- length(which(tmp > 10))
p <- x/length(tmp)
p
#0.01428
    #This is pretty close to the value 
    #we get from integrating, 0.0147976.
