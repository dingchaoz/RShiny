## Lexical scoping : Function returning a function example
x<- make.power(n){
  ## Returns a function pow
  pow <- function(x){
    ## n is a free variable in the function pow
    x^n}
  pow
}