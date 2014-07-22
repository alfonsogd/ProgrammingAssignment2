## Put comments here that give an overall description of what your
## functions do
## This pair of functions are supposed to to create a special object that 
## stores a matrix and caches its inverse.

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special object, which is 
## a list containing a function to 
## set the matrix
## get the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL   
        set <- function (y) {
                x <<- y          
                m <<- NULL   
        }
        get <- function() x
        setmatrixinverse <- function(inverse) m <<- inverse
        getmatrixinverse <- function() m
        list(set = set, get = get,
        setmatrixinverse = setmatrixinverse,
        getmatrixinverse = getmatrixinverse)
}

## Write a short comment describing this function
##The following function calculates the matrix created with the above function, 
## it first checks to see if the matrix inverse has already been calculated. If 
## so, it gets the matrix inverse from the cache and skips the computation. 
## Otherwise, it calculates the matrix inverse of the input matrix and sets 
## the matrix inverse in the cache via the setmatrix function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        m <- x$getmatrixinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrixinverse(m)
        m
}