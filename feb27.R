f <- function(x){x*exp(-x)}
Df <- function(x){1*exp(-x) - x*(exp(-x))}
D2f <- function(x){-2*exp(-x) + x*exp(-x)}

uniroot(Df,c(-10,10))$root

Df(uniroot(Df,c(-10,10))$root)
Df(1)

D2f(1)

x <- seq(-10,10,0.001)
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f(x),type = "l")

par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f(x),type = "l",xlim=c(-1,10),ylim=c(-3,1))


my_plot <- function(f,left_bound,right_bound,gap = (right_bound-left_bound)/100, mar = c(2.5,2.5,0.25,0.25), type = "l",... ){
  x <- seq(left_bound,right_bound,gap)
  par(mar=mar)
  plot(x,f(x),type = type,...)
}
my_lines <- function(f,left_bound,right_bound,gap = (right_bound-left_bound)/100, type = "l",... ){
  x <- seq(left_bound,right_bound,gap)
  lines(x,f(x),type = type,...)
}



g <- function(x){x*(1-x)}
Dg <- function(x){1 - 2*x}
D2g <- function(x){0*x - 2}

cv <- uniroot(Dg,c(-10,10))$root

Dg(cv)

D2g(cv)