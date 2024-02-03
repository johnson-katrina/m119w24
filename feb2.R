###Likelihood Graph Example 1
rm(list=ls())
###Define the distribution###
p <- function(x,lambda=1){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

###Define the likelihood function###
LP <- function(lambda,x){
  # The element of x must be a whole numbers.
  prod(p(x,lambda))
}

###Possible Parameter Values###
lambda <- seq(0,10,0.001)

###Data###
# Florida Hurricane Data (2000-2022)
data <- c(4,4,8,8,6,8,2,8,8,4,8,6,4,3,2,4,6,7,4,7,13,3,3)

#Here we calculate the output from the likelihood function given the observed data.
y <- sapply(lambda,FUN=LP,x=data)

#We plot the likelihood function.
par(mar=c(2.5,2.5,3,0.25))
plot(lambda,y,type='l',main='Poisson Likelihood')



###Likelihood Graph Example 2
###Define the distribution###
f2 <- function(x,lambda=1){
  # x and lambda must be positive
  lambda*exp(-lambda*x)
}

###Define the Likelihood function###
LE <- function(lambda,x){
  # The elements of x must be positive.
  prod(f2(x,lambda))
}

###Possible Parameter Values###
lambda <- seq(0,10,0.001)

###Data###
# Some Simulated Data (This is data from an Exponential random variable.)
data <- c(0.45729967, 0.47156107, 1.21461705, 0.20539769, 1.78975399, 0.09095850, 0.64675475, 1.60109333, 1.57752679, 0.01238945)

#Here we calculate the output from the likelihood function given the observed data.
y <- sapply(lambda,FUN=LE,x=data)

#We plot the likelihood function.
par(mar=c(2.5,2.5,3,0.25))
plot(lambda,y,type='l',main='Exponential Likelihood')



###Likelihood Graph Example 3
###Define the distribution###
f3 <- function(x,mu=0,s=1){
  (1/sqrt(2*pi*s^2))*exp(-(x-mu)^2/(2*s^2))
}

###Define the likelihood function###
# For simplicity, we'll assume sigma is 1.
LN <- function(mu,sigma=1,x){
  prod(f3(x,mu,sigma))
}

###Possible Parameter Values###
mu <- seq(-10,10,0.001)

###Data###
# Some more Simulated Data (This is data from a Normal random variable.)
data <- c(-3.77117676, -2.91429587, -2.02774901, -0.23984575, -1.41960740, -3.17490528, -3.21755276, -0.06442566, -1.92134953, -0.93160739)


#Here we calculate the output from the likelihood function given the observed data.
y <- sapply(mu,FUN=LN,x=data,sigma=1)

#We plot the likelihood function.
par(mar=c(2.5,2.5,3,0.25))
plot(mu,y,type='l',main='Normal Likelihood')

