## Lexical scoping vs Dynamica Scoping
y<-10
f<- function(x){
        ## here y and g(x) are free variables
        y <- 2
        y^2 + g(x)
}

g <- function(x){
        x*y
        ## where y is the free variable in the function g
}

## question that is of interest is what is f(3)
## Before execution I am guessing it should be an error - because the g(x)
## does not exist at the time of search. Lets execute
## Hmmmm....Had I executed f(3) in line 8 before g<-function(x)...
## there would be an error. See below
## > f(3)
## Error in f(3) : could not find function "g"
## In lexical scoping the value of "y" is taken from the global initiation and
## and not as initialized witin function "f"

f(3)
