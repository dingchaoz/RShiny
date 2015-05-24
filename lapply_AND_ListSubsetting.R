## Learning lapply function
## like matlab cellfun or arrayfun. Applies a function to each element of a list.

x<-list(a=1:15,b=rnorm(10))
ans<- lapply(x,mean)
ans

## returns a list as an answer.
## always returns a list; inputs are coerced into a list.

## lets look at a function called runif (random uniform)
runif(1)

##  runif function (n, min = 0, max = 1) - takes n as the number of random #s to generate
## between the min and the max ; the default min and the max are 0 and 1 respectively.

A <- 1:4
resp <- lapply(A,runif)
resp

## the above code returns a list with each successive elements of the list being
#3 runif(1),runif(2),runif(3),runif(4). 

resp2<- lapply(A,runif,min=0,max=10)
## the above line of code allows you to provide arguments for runif in lapply
resp2

## use of annonymous functions
## again similar to cellfun in Matlab

## The below line of code is an attempt to get the first and the second element of a list

first2 <- lapply(resp2 , function(lst) lst[1:2])
first2

## Subsetting from a list lecture 1 video
## if you want the first element from the second elemrnt of the lsit first2

first[[c(2,1)]]

## NOW LETS LOOK AT SAPPLY 
## sapply simplifies the output of an lapply. In cases where the output would best
## as an vector or matrix

first2 <- sapply(resp2,function(lst) lst[1:2])
first2 ## notice that the lapply returned first2 as a matrix of (2 by 4),
## first 2 elements of the 4 elements of the list resp2
