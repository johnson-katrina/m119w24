#inputs
x <- seq(-4,20,0.1)
#outputs
y <- 10-sqrt(x+4)
#plotting the function (or model)
par(mar=c(2.5,2.5,0.5,0.5))
plot(x,y,type='l')

library(data4led)
dist <- led_time(2100)
hist(dist$percent_intensity, probability = TRUE)

rm(list=ls())
f0 <- function(L,a=0,b=1){
  # Make sure a < b when using this function.
  ifelse(L < a,NaN, ifelse(L <= b, 1/(b-a), NaN))
}

a <- 100
b <- 103
L <- seq(a,b,0.1)
y <- f0(L,a,b)

par(mfrow=c(1,2),mar=c(2.5,2.5,1,0.25))
plot(L,y,type='l',xlim=c(90,110), ylim = c(0,1))
mtext('For f0: a=100, b=103', side = 3, line = 0)

a <- 91
L <- seq(a,b,0.1)
y <- f0(L,a,b)

plot(L,y,type='l',xlim=c(90,110), ylim = c(0,1))
mtext('change a=91 (keep b=103)', side = 3, line = 0)


par(mfrow=c(1,1),mar=c(2.5,2.5,1,0.25))
hist(dist$percent_intensity, probability = TRUE)
a <- 100
b <- 103
L <- seq(a,b,0.1)
y <- f0(L,a,b)

lines(L,y,type='l',xlim=c(90,110), ylim = c(0,1))