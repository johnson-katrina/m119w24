#This script file includes the code from class Jan 11.

#Make a list of numbers (a vector).
val <- c(-2,-1,0,1,2,3,4,5)
#This is another list of numbers (a vector).
val2 <- seq(-2,5,0.5)
#Notice that val and val2 are the same (the vectors have the same values) even though they were created using different commands.

#When we square a vector in R, every element of the vector is squared.
val^2

#When we plot a vector in R, the points will be the (index, value) for each elment in the vector.
#R starts counting at 1 (note that this is different than Python).
plot(val)

#We can plot input-output pairs in R.
#The vector of inputs is a first argument entered into the plot function.
#The vector of outputs is the second argument entered into the plot function.
plot(val,val^2)

#The default for the plot() command in R is a plot of points (a scatterplot).
#If we want to plot a curve we specify 'l' as the type to create a line plot.
plot(val,val^2,type='l')
#Since val and val2 have the same values we notcie that using either creates the same plot.
plot(val2,val2^2,type='l')

#We can create our own functions in R with the following syntax.
f <- function(x){
  x^2
}

#We are evaluating the function f(x) = x^2 for each of the values in the vector val.
f(val)
