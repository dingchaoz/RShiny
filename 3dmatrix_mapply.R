## Creating 3 dimentional matricies...Intersting!

aa<- rnorm(2*2*10)
str(aa)
c<-array(aa,c(4,5,2))
rowMeans(c,dim=2)

## mapply :- used to supply multiple lists for several arguments in the function as in the below wxample

noise <- function(n,mean,sd){
        rnorm(n,mean,sd)
}

noise(5,1,7)

## Also look at the output of the below function

noise(1:5,1:5,2)
## the above line of code doesn't work if we want
## results for mean 1 thru' 5 and number of random numbers from 1 thu' 5
## hence a mapply function can come in handy

resp <- mapply(noise,1:5,1:5,2)
resp

