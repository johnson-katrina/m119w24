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


#Brain Gains
f <- function(x){5-x}
draw_rug(f,2,5)

f <- function(x){x-1}
draw_rug(f,1,3)


g <- function(x){x^2*exp(-x)}
a <- 1
b <- 4
n <- 6
draw_rect_approx(g,a,b,n,method = "right")
dx <- (b-a)/n
xi <- seq(a+dx,b,dx)  #Can you tell what part of this line of code has us use right endpoints?
data.frame(right_point = xi, function_at_xi = g(xi), area_i = g(xi)*dx)
c(riemann_sum_using_right_endpoints = sum(g(xi)*dx))

n <- 10000000
dx <- (b-a)/n
xi <- seq(a+dx,b,dx)  
A <- sum(g(xi)*dx)
A

f <- function(x){1/A*g(x)}
a <- 1
b <- 4
n <- 10000000
dx <- (b-a)/n
xi <- seq(a+dx,b,dx)  #Can you tell what part of this line of code has us use right endpoints?
c(riemann_sum_using_right_endpoints = sum(f(xi)*dx))

EV <- sum(xi*f(xi)*dx)
EV

Var <- sum((xi-EV)^2*f(xi)*dx)
Var

f <- function(x){1/A*g(x)}
a <- 1
b <- 3
n <- 10000000
dx <- (b-a)/n
xi <- seq(a+dx,b,dx)  #Can you tell what part of this line of code has us use right endpoints?
c(riemann_sum_using_right_endpoints = sum(f(xi)*dx))

f <- function(x){1/A*g(x)}
a <- 2
b <- 3
n <- 10000000
dx <- (b-a)/n
xi <- seq(a+dx,b,dx)  #Can you tell what part of this line of code has us use right endpoints?
c(riemann_sum_using_right_endpoints = sum(f(xi)*dx))
