f <- function(x){
  ifelse(x < -2, -x -4,
         ifelse(x <= 2, 0*x -2, x-4))
}

x <- seq(-10,10,0.1)
y <- f(x)

plot(x,y,type='l',ylim=c(-10,10))
abline(h=0,col="gray")

k <- c(2,3,4)
sum(2*k-3)

prod(2*k-3)
