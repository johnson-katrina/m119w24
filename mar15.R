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


#Brain Gains Q1 Plots
g <- function(x){
  #This function is only programmed to take values from 0 to 5.
  ifelse(x<2,4,1)
}
draw_rug(g,0,4,num_points = 1000)
draw_rect_approx(g,0,4,8)

#Calculating Expected Value (and Area) using Rectangular Approximations
g <- function(x){2*x}
a <- 0
b <- 5
n <- 5
dx <- (b-a)/n
draw_rect_approx(g,a,b,n) 

xi <- seq(a+dx/2,b,dx)  #Center of each rectangle
Ai <- g(xi)*dx          #Area of each rectangle
data.frame(x_i = xi, A_i = Ai)

c(total_area =sum(Ai),  
  expected_value = sum(xi*Ai)/sum(Ai))

    #Increase number of rectangles (the value for n)
g <- function(x){2*x}
a <- 0
b <- 5
n <- 10
dx <- (b-a)/n
draw_rect_approx(g,a,b,n) 
xi <- seq(a+dx/2,b,dx)  #Center of each rectangle
Ai <- g(xi)*dx          #Area of each rectangle
#data.frame(x_i = xi, A_i = Ai)
c(total_area =sum(Ai),  
  expected_value = sum(xi*Ai)/sum(Ai))

    #Even more rectangles
n <- 1000
dx <- (b-a)/n
draw_rect_approx(g,a,b,n) 
xi <- seq(a+dx/2,b,dx)  #Center of each rectangle
Ai <- g(xi)*dx          #Area of each rectangle
#data.frame(x_i = xi, A_i = Ai)
c(total_area =sum(Ai),  
  expected_value = sum(xi*Ai)/sum(Ai))
#We see the approximate expected value approaches 
    #the exact expected value E[X]=10/3 calculated
    #using the centroid formula for a right triangle.




#Another function.
g <- function(x){x^2}
a <- 0
b <- 5
n <- 100
dx <- (b-a)/n
#draw_rect_approx(g,a,b,n) 
xi <- seq(a+dx/2,b,dx)  #Center of each rectangle
Ai <- g(xi)*dx          #Area of each rectangle
#data.frame(x_i = xi, A_i = Ai)
c(total_area =sum(Ai),  
  expected_value = sum(xi*Ai)/sum(Ai))

#A third function (this function is not on our centroid charts).
g <- function(x){x^3}
a <- 0
b <- 4
n <- 100
dx <- (b-a)/n
#draw_rect_approx(g,a,b,n) 
xi <- seq(a+dx/2,b,dx)  #Center of each rectangle
Ai <- g(xi)*dx          #Area of each rectangle
#data.frame(x_i = xi, A_i = Ai)
c(total_area =sum(Ai),  
  expected_value = sum(xi*Ai)/sum(Ai))
total_area =sum(Ai)
    #To calculate P(X <= 3) we also need area left of 3.
a <- 0
b <- 3
n <- 100
dx <- (b-a)/n
#draw_rect_approx(g,a,b,n) 
xi <- seq(a+dx/2,b,dx)  #Center of each rectangle
Ai <- g(xi)*dx          #Area of each rectangle
#data.frame(x_i = xi, A_i = Ai)
total_area_left3 =sum(Ai)
    #P(X <= 3)
total_area_left3/total_area



#A final function (this function is not on our centroid charts).
g <- function(x){5*x*exp(-x)}
a <- 2
b <- 8
n <- 100
dx <- (b-a)/n
#draw_rect_approx(g,a,b,n) 
xi <- seq(a+dx/2,b,dx)  #Center of each rectangle
Ai <- g(xi)*dx          #Area of each rectangle
#data.frame(x_i = xi, A_i = Ai)
c(total_area =sum(Ai),  
  expected_value = sum(xi*Ai)/sum(Ai))
total_area =sum(Ai)
#To calculate P(X <= 3) we also need area left of 3.
a <- 2
b <- 3
n <- 100
dx <- (b-a)/n
#draw_rect_approx(g,a,b,n) 
xi <- seq(a+dx/2,b,dx)  #Center of each rectangle
Ai <- g(xi)*dx          #Area of each rectangle
#data.frame(x_i = xi, A_i = Ai)
total_area_left3 =sum(Ai)
#P(X <= 3)
total_area_left3/total_area