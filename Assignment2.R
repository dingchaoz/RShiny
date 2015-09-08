## Programing assignment 2
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
# 
# Write the following functions:
#         
#         makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

# For this assignment, assume that the matrix supplied is always invertible.

## Author : Srivathsan Seshadri, 2015-05-25 (memorial day weekend!)

# MakeCacheMAtrix intends to putput a list of 4 functions:
#       1. set : intends to set new matrix of which inverse need to be computed.
#       2. get : intends to get the matrix of which the inverse need to be computed.
#       3. srtInv: intends to compute the inverse.
#       4. getInv : internds to retrieve the cached inverse.
makeCacheMatrix <- function (x=matrix()){
        
        Inv <- matrix() # sets a empty matrix
        set <- function(y){
                x <<- y # assigns y to x (y is a free matrix variable; y would be entered by user for which inverse needs to be computed)
                Inv <<- matrix() # sets a empty matrix
        }
        get <- function() {
#                 if (is.na(x)!=T){
#                         x # output x      
#                 }
#                 else {
#                         warning("Attempting to get an empty matrix; inverse of empty matrix not possible")
#                 }
                x
        }
        setInv <- function(Inv){
                Inv <<- solve(x) # computes the inverse and assigns it to Inc
        }
        getInv <- function(){
                Inv
        }
        list (set = set, get = get, setInv = setInv, getInv = getInv) # output list
}

cacheSolve <- function(x,...){
        Inv <- x$getInv() # getting cached inverse
        if (!is.na(Inv)){
                message("getting cached inverse")
                return(Inv)
        }
        data <- x$get() # calls "get" function 
        Inv <- solve(data)
        x$setInv(Inv)
        Inv
        
}

