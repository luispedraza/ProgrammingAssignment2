## This is the assignment for weeb 3 of Coursera R Programming course
## This file contains two functions:

## This function creates a special "matrix" object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL # this is the inverse matrix value
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverseValue) inverse <<- inverseValue
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix x, which is the parameter
## If the inverse has been previously computed, returns the cached value
## Else, computes the inverse and caches the value inside the special matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    value <- x$getInverse()
    if (!is.null(value)) {
        message("getting cached data")
        return (value)
    }
    
    # Else, we need to compute the inverse value:
    theMatrix <- x$get()
    value <- solve(theMatrix)
    x$setInverse(value)
    # And finally, return the inverse value:
    value
}

## This is for testing:
m <- matrix(c(2, 0, 0, 0, 2, 0, 0, 0, 2), 3, 3)
print(m)
specialM <- makeCacheMatrix(m)

inverse <- cacheSolve(specialM)
print(inverse)
# Now, cached version
inverse <- cacheSolve(specialM)
print(inverse)
