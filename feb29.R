#Between Class Work Question (Level 0)
b11 <- 3
b12 <- 2
b21 <- 2
b22 <- 5
c1 <- 7
c2 <- 12

x <- (c1*b22 - b12*c2)/(b11*b22 - b12*b21)
y <- (b11*c2 - c1*b21)/(b11*b22 - b12*b21)

#Check
b11*x+b12*y == c1
b21*x+b22*y == c2

#Graphical Check
line1 <- function(x){(c1 - b11*x)/b12}
line2 <- function(x){(c2 - b21*x)/b22}
inputs <- seq(x-10,x+10,0.1)
outputs_line1 <- line1(inputs)
outputs_line2 <- line2(inputs)
plot(inputs,outputs_line1,type="l")
lines(inputs,outputs_line2,col=3)
points(x,y,pch=16)



#Brain Gains Question 2
    #To use the results from our Between Class work, we must identify b11, b12, b21, b22, c1, and c2.
b11 <- pi
b12 <- log(2)
i <- c(1,2,3)
b21 <- sum(i^2)
b22 <- sum(i-1)
c1 <- 7
c2 <- sum(i-i^2)

x <- (c1*b22 - b12*c2)/(b11*b22 - b12*b21)
y <- (b11*c2 - c1*b21)/(b11*b22 - b12*b21)
x
y

#Check
b11*x+b12*y == c1
b21*x+b22*y == c2

#Graphical Check
line1 <- function(x){(c1 - b11*x)/b12}
line2 <- function(x){(c2 - b21*x)/b22}
inputs <- seq(x-10,x+10,0.1)
outputs_line1 <- line1(inputs)
outputs_line2 <- line2(inputs)
plot(inputs,outputs_line1,type="l")
lines(inputs,outputs_line2,col=3)
points(x,y,pch=16)



#Brain Gains Question 3
    #To use the results from our Between Class work, we must identify b11, b12, b21, b22, c1, and c2.
    #We use the properties of derivatives and sums to rewrite.
n <- seq(1,44)
b11 <- sum(n)
b12 <- 3*44
b21 <- 44*5
b22 <- b11
c1 <- 44*7
c2 <- sum(n^2)

#The "L" does not refer to the Roman Numeral system, 990 multiplied by L (the number 50).
    #That would be ridiculous!
    #As a sanity check we also can see that b11 must be smaller than c2.
        #Since c2 is the sum of the numbers 1 to 44 squared and b11 is the sum of the numbers 1 to 44.
#So, what does the "L" mean when we read in the value of b11 is "990L"?
    #Rather the "L" tells us that R has stored b11 as an integer.
    #R is storing b11 as an integer because it takes less storage space.
#We can also see that when we type b11 in the Console we see it is 990.
b11
#We can use the class() function in R to tell us how a particular variable has been saved.
    #We see b11 is saved as an integer value.
    #We see b12 is saved as a numeric value.
class(b11)
class(b12)

x <- (c1*b22 - b12*c2)/(b11*b22 - b12*b21)
y <- (b11*c2 - c1*b21)/(b11*b22 - b12*b21)
x
y

#Check
    #This check doesn't look like it worked. R says these statements are FALSE.
    #This is because R is using floating point arithmetic for the computations.
b11*x+b12*y == c1
b21*x+b22*y == c2

#Graphical Check
    #Our graphical check does help us see that the solution we found is correct.
line1 <- function(x){(c1 - b11*x)/b12}
line2 <- function(x){(c2 - b21*x)/b22}
inputs <- seq(x-10,x+10,0.1)
outputs_line1 <- line1(inputs)
outputs_line2 <- line2(inputs)
plot(inputs,outputs_line1,type="l")
lines(inputs,outputs_line2,col=3)
points(x,y,pch=16)