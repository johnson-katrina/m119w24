solvesystem <- function(c11, c12, b1, c21, c22, b2){
  c((b1*c22 - b2*c12)/(c11*c22 - c21*c12), 
    (c11*b2 - c21*b1)/(c11*c22 - c21*c12))
}

solvesystem(3, 2, 7, 2, 5, 12)

library(data4led)
bulb <- led_bulb(1,seed=123)
t <- bulb$hours
y <- bulb$percent_intensity

c.11 <- sum(t^2)
c.12 <- sum(t^3)
b.1 <- sum((y-100)*t)
c.21 <- c.12
c.22 <- sum(t^4)
b.2 <- sum((y-100)*t^2)

solvesystem(c.11,c.12,b.1,c.21,c.22,b.2)
