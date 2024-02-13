rm(list=ls())
f <- function(x){x^2-x}

x <- seq(-10,10,0.001)
y <- f(x)

plot(x,y,type='l')

tangent_line <- function(x){6 - 5*(x+2)}
tangent_line_v2 <- function(x){-5*(x+2) + 6}
tangent_line_outputs <-tangent_line(x)

plot(x,y,type='l')
lines(x,tangent_line_outputs)
points(-2,f(-2))
