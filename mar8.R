rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data5_ls.csv"))
x <- data$x
y <- data$y

a.best <- sum(y*log(x))/sum((log(x))^2)

a.best

#The fitted model is f(x) = 11.608ln(x).

#Visualize the fit.
f <- function(x,a=a.best){a*log(x)}
xvals <- seq(min(x),max(x),0.1)
plot(x,y,pch=16)
lines(xvals,f(xvals),col=2)

#Answer the Question
    #What value is predicted by the model at x=4?
f(4)
#16.09239

#Answer the Question
    #When is the output of the model predicted to be 5?
    #We need to solve f(x) = 5. Let's use uniroot().
f.sol <- function(x){f(x)-5}
uniroot(f.sol,c(1,4))$root

sol <- uniroot(f.sol,c(1,4))$root

#Visualize Answer
plot(x,y,pch=16)
lines(xvals,f(xvals),col=2)
points(4,f(4),col=3)
abline(h=5,v=sol,col='gray')
