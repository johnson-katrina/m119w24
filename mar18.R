#######
#These first functions are copied from the top of the 
#Rectangles, Rugs, and Riemann Sums reading
#######
library(knitr)

#Shades a rug diagram for a probability mass function. 
#Inputs: 
#  x - a vector of data points
#  p - a corresponding vector of probabilities or frequencies
#All widths are 1 unit wide. 
draw_pmf <- function(x,p){
  xs <- c(rbind(x-1/2,x-1/2,x+1/2,x+1/2))
  px <- c(rbind(0,p,p,0))
  par(mar=c(2.5,2.5,0.25,0.25))
  plot.new()
  plot(xs,px,type="l")
  polygon(xs,px,col="gray")
}

#Shades a rug diagram (shades area under) for a function f from a to b. 
#Inputs: 
#  f - a function f(x)
#  a - left end of the rug
#  b - right end of the rug
#  num_points - how many point are sent into f for plotting. 
draw_rug <- function(f,a,b,num_points=100){
  x <- c(a,seq(a,b,(b-a)/num_points),b,a)
  y <- c(0,f(seq(a,b,(b-a)/num_points)),0,0)
  par(mar=c(2.5,2.5,0.25,0.25))
  plot(x,y,type = "l")
  polygon(x,y,col="gray")
}

#Draws rectangles over the top of a given function.
#The midpoint of top of each rectangle passes through the function. 
#  f - a function f(x)
#  a - left end of graph
#  b - right end of graph
#  num_rectangles - how many rectangles to plot.
#  method - One of "left", "right", or "mid".  Defaults to mid.
draw_rect_approx <- function(f,a,b,num_rectangles, method = "mid"){
  n <- num_rectangles
  dx <- (b-a)/n
  x <- c(a,seq(a,b,dx/100),b,a)
  y <- c(0,f(seq(a,b,dx/100)),0,0)
  par(mar=c(2.5,2.5,0.25,0.25))
  plot(x,y,type = "l")
  
  if(method == "left"){
    xi <- seq(a+0*dx/2,b-dx/2,dx)
    lines(xi,f(xi),type = "h")
    lines(xi,f(xi),type = "s")
    lines(c(xi[n],xi[n]+dx),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n],xi[n]+dx),f(c(xi[n],xi[n])),type = "h")
  }
  else if(method == "right"){
    xi <- seq(a+dx,b+dx/2,dx)
    lines(xi-dx,f(xi),type = "h")
    lines(xi-dx,f(xi),type = "s")
    lines(c(xi[n]-dx,xi[n]),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n]-dx,xi[n]),f(c(xi[n],xi[n])),type = "h")
  } 
  else{#Use midpoint
    xi <- seq(a+dx/2,b,dx)
    lines(xi-dx/2,f(xi),type = "h")
    lines(xi-dx/2,f(xi),type = "s")
    lines(c(xi[n]-dx/2,xi[n]+dx/2),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n]-dx/2,xi[n]+dx/2),f(c(xi[n],xi[n])),type = "h")
  }
}
#######





f0 <- function(x,a=0,b=1){(1/(b-a))+0*x}

x <- seq(-1,2,0.1)
y <- f0(x,-1,2)

plot(x,y,type='l',xlim=c(-2,3),ylim=c(0,0.5))

set.seed(123)
tmp <- runif(25000,-1,2)

count <- length(which(tmp<=1.2))
count
count/length(tmp)

(1/3)*(1.2+1)

hist(tmp,probability = TRUE,xlim=c(-2,3),ylim=c(0,0.5))
lines(x,y,col=2)



g <- function(x){4-x^2}
a <- -1
b <- 2
n <- 10
dx <- (b-a)/n
draw_rect_approx(g,a,b,n) 

#Start at half of dx to the right of a, and then step by dx.
xi <- seq(a+dx/2,b,dx)
points(xi,g(xi),pch=16,col=2)
segments(xi,rep(0,length(xi)),xi,g(xi),col=2)

Ai <- g(xi)*dx
sum(Ai)

draw_rect_approx(g,a,b,n,method='left') 

#Start a, and then step by dx.
xi <- seq(a,b-dx,dx)
points(xi,g(xi),pch=16,col=2)
segments(xi,rep(0,length(xi)),xi,g(xi),col=2)

Ai <- g(xi)*dx
sum(Ai)


draw_rect_approx(g,a,b,n,method='right') 

#Start a plus dx, and then step by dx.
xi <- seq(a+dx,b,dx)
points(xi,g(xi),pch=16,col=2)
segments(xi,rep(0,length(xi)),xi,g(xi),col=2)

Ai <- g(xi)*dx
sum(Ai)

#Try n=10, 75, 100, 1000, 50000, 750000, 4000000
g <- function(x){4-x^2}
a <- -1
b <- 2
n <- 10
dx <- (b-a)/n

#Mid: Start at half of dx to the right of a, and then step by dx.
xi.m <- seq(a+dx/2,b,dx)
#Left: Start a, and then step by dx.
xi.L <- seq(a,b-dx,dx)
#Start a plus dx, and then step by dx.
xi.R <- seq(a+dx,b,dx)

Ai.m <- g(xi.m)*dx
Ai.L <- g(xi.L)*dx
Ai.R <- g(xi.R)*dx
sum(Ai.m)
sum(Ai.L)
sum(Ai.R)