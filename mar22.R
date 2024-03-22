#Load the Data
library(data4soils)
Ng <- cfbp_fpjuliet$ng

#Calculate the mean and variance of the data.
m<-mean(Ng)
v<-var(Ng)

#Using the Method of Moments we find mu mean of the data and sigma is calculated below.
s<-sqrt(v)

#Using the Method of Moments we solve the system of equation to calculate alpha and beta.
alpha <- m^2/v 
alpha
beta <- m/v
beta

###The following code from the end of class has an ERROR.
#The ERROR is in the function definition of f1.
#The e to the power factor has the square outside rather than inside where it should be.
f1 <- function(x,m=0,s=1){1/sqrt(2*pi*s^2) * exp(-1/2*((x-m)/s))^2}
f2 <- function(x,a=1,b=1){b^a/gamma(a)*(x)^(a-1)*exp(-b*x)}

x <- seq(-20,20,0.1)
hist(Ng,probability = TRUE)
lines(x,f1(x,m,s),col=2)

x <- seq(0,20,0.1)
lines(x,f2(x,alpha,beta),col="blue")


###The following code has the error in the definition of f1 corrected.
#The square is in the right spot.
f1 <- function(x,m=0,s=1){1/sqrt(2*pi*s^2) * exp(-1/2*((x-m)/s)^2)}
f2 <- function(x,a=1,b=1){b^a/gamma(a)*(x)^(a-1)*exp(-b*x)}

x <- seq(-20,20,0.1)
hist(Ng,probability = TRUE)
lines(x,f1(x,m,s),col=2)

x <- seq(0,20,0.1)
lines(x,f2(x,alpha,beta),col="blue")

###The following code has the functions f1 and f2 copied from our Day 38 (March 21) on our Wiki.
#In the section Project 3 and Knewton Alta Tips.
f1 <- function(x,m=0,s=1){1/(sqrt(2*pi*s^2))*exp(-0.5*((x-m)/s)^2)}
f2 <- function(x,a=1,b=1){b^a/gamma(a)*(x)^(a-1)*exp(-b*x)}

x <- seq(-20,20,0.1)
hist(Ng,probability = TRUE)
lines(x,f1(x,m,s),col=2)

x <- seq(0,20,0.1)
lines(x,f2(x,alpha,beta),col="blue")
