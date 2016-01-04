## split function
## split takes a vector or other objects and splits it into groups determined by the factor or list of factors

str(split)
## above line yields function (x, f, drop = FALSE, ...) 

#3 where x is the vector that needs to be split
## f is a vector of same length of x
## drop is an argument where it allows you drop those cases where there is no observation in X for a particular
## factor setting f. Think of a DOE where there is no observation for a particular factor setting.

x<-c(rnorm(10),runif(10),rnorm(10,2)) ## generate 30 random numbers; 10 each from 3 different distributions
f<-gl(3,10) ## use gl function to generate 3 levels of size 10 each (k =3 ; n = 10 Donlad J Wheeler's notation

split(x,f)

## Here is an experiment trying to use the airquality data... tried to get the means of every column in airquality data
## by month... notice tapply doesn't work as it takes only a vector as x

library(datasets)
> head(airquality)
# Ozone Solar.R Wind Temp Month Day
# 1    41     190  7.4   67     5   1
# 2    36     118  8.0   72     5   2
# 3    12     149 12.6   74     5   3
# 4    18     313 11.5   62     5   4
# 5    NA      NA 14.3   56     5   5
# 6    28      NA 14.9   66     5   6
# > response <- tapply(airquality,airquality$Month,mean)
# Error in tapply(airquality, airquality$Month, mean) : 
#         arguments must have same length
# > airquality$Month
# [1] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
# [25] 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
# [49] 6 6 6 6 6 6 6 6 6 6 6 6 6 7 7 7 7 7 7 7 7 7 7 7
# [73] 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 8 8 8 8
# [97] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
# [121] 8 8 8 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
# [145] 9 9 9 9 9 9 9 9 9
# > dim(airquality)
# [1] 153   6
# > response <- tapply(airquality,airquality$Month,colMeans,simplify=T)
# Error in tapply(airquality, airquality$Month, colMeans, simplify = T) : 
#         arguments must have same length
# > response <- lapply(airquality,airquality$Month,colMeans,simplify=T)
# Error in match.fun(FUN) : 
#         'airquality$Month' is not a function, character or symbol
# > response <- tapply(airquality$Ozone,airquality$Month,mean)
# > response
# 5  6  7  8  9 
# NA NA NA NA NA 
# > response <- tapply(airquality$Ozone,airquality$Month,mean,na.rm=T)
# > response
# 5        6        7        8        9 
# 23.61538 29.44444 59.11538 59.96154 31.44828 

## However, if you use a split function to split the data frame by month it becomes easy to calculate 
## means by using lapply function

s<- split(airquality,airquality$Month)
response <- lapply(s,function(x) colMeans(x[,c("Ozone","Solar.R","Wind")]))
response
response <- sapply(s,function(x) colMeans(x[,c("Ozone","Solar.R","Wind")]))
response

