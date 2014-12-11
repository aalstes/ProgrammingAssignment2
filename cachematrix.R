## The functions makeCacheMatrix and cacheSolve are used to calculate
## the inverse of a matrix and store the calculated inverse in a cache.
## Example usage:
##
## A <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
## cm <- makeCacheMatrix(A)
## cacheSolve(cm)
## Returns:
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## The first time the inverse is calulated using the function solve.
## After that the cached inverse is returned. If a new vaule is set
## for the matrix, its inverse is reset to null.

## Creates a list of functions that can be passed to the
## cacheSolve function.
## Takes as its argument the matrix whose inverse we want to
## store in the cache.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL # s is the inverse (solve). Initialized as null.
        # set allows setting a new value for the matrix (resets the inverse).
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        # get allows getting the matrix.
        get <- function() x
        # setsolve allows storing a value for the inverse matrix.
        setsolve <- function(solve) s <<- solve
        # getsolve allows getting the inverse matrix.
        getsolve <- function() s
        # Returns a list of getters and setters for the matrix
        # and its inverse.
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Returns a matrix that is the inverse of the matrix passed to makeCacheMatrix.
## Takes as its argument a list of functions created
## by the function makeCacheMatrix.
## Tries to get the inverse from the cache using x$getsolve().
## Returns the cached inverse, if not null. Otherwise calculates
## the inverse and stores it in the cache and finally returns it.

cacheSolve <- function(x, ...) {
        # Get from cache:
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                # Return inverse from cache:
                return(s)
        }
        # null in cache. Calculate inverse and store in cache:
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        # Return inverse:
        s
}
