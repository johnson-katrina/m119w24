rm(list=ls())

#Inputs
x <- seq(-20,20,0.1)
#Outputs LHS
y <- x^2+3*x
#Outputs RHS
y1 <- rep(12,length(x))

#Plot of LHS and RHS
plot(x,y,type='l')
lines(x,y1)
points(c((-3+sqrt(57))/2,(-3-sqrt(57))/2),c(12,12),pch=16,col="green")

#Approx. values of solutions
(-3+sqrt(57))/2
(-3-sqrt(57))/2