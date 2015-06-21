
## Implements a cache to store the inverse of a matrix

## Creates a list of functions to store/retrieve a matrix and it's inverse
## Argument, `m': a matrix
makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    
    # gets the value of the matrix
    get <- function() m
    
    # sets the value of the matrix
    set <- function(x) {
        m <<- x
        inv <<- NULL
    }
    
    # gets the value of the inverse matrix
    getInverse <- function() inv
    
    # sets the value of the inverse matrix
    setInverse <- function(x) inv <<- x
    
    # return a list exposing the inner functions
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}

## Computes the inverse of the matrix enclosed by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache
## Argument `x': a makeCacheMatrix list containing a matrix `m'
## Returns the inverse matrix of `m'
cacheSolve <- function(x, ...) {

    # check if inverse present in cache
    inv <- x$getInverse()
    if (is.null(inv)) {
        # compute the inverse by calling base function 'solve'
        message("computing inverse...")
        inv <- solve(x$get(), ...)
        x$setInverse(inv)
    }
    
    # return the inverse
    inv
}

