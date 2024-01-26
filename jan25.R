log(37,2)

?uniroot

rm(list=ls()) #Clears the environment

h <- function(x){
  3*x -15
}

#What does the $root code do below?
uniroot(h,c(0,30))
uniroot(h,c(0,30))$root

x <-seq(0,30,1)
plot(x,h(x), type="l")
abline(h=0, col = "lightgray")

h.shift <- function(x){
  h(x) - 4
}

uniroot(h.shift,c(0,5))$root
h.shift(0)
h.shift(5)
uniroot(h.shift,c(0,10))$root


f <- function(x){
  1/x
}

uniroot(f,c(-10,-3))$root
uniroot(f,c(-1,1))$root
