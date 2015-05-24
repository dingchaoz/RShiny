## tapply is used to apply a function (even anonymous functions) to subsets of a vector.
## Think of an excel pivot table where you calculate a statistic (mean, stdev, range, or any function)
## for subgroups of data.
## look at the output of tapply
## tapply
## function (X, INDEX, FUN = NULL, ..., simplify = TRUE) 
## where x is a vector; index is a vector of the same length as x
## FUN is any function that you want executed on X anf the "..." is additional arguments for the function passed.
## simplify = true; simplifies the output of the tapply function into a vector or matrix.
## When simplify is set to false - the output is a list.

## Example as shown below

x<-c(rnorm(10),runif(10),rnorm(10,2)) ## generate 30 random numbers; 10 each from 3 different distributions
f<-gl(3,10) ## use gl function to generate 3 levels of size 10 each (k =3 ; n = 10 Donlad J Wheeler's notation)
f
resp <- tapply(x,f,mean)
resp
resp <- tapply(x,f,mean,simplify = FALSE)
resp
