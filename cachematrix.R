## Creates a special "matrix" that can cache its inverse.
## Function Will read cached data first to see if 
## inverse has already been calculated.
## Reads from cache if so. Otherwise, calculates inverse.

## creates a special "matrix" object that can cache its inverse.
## The function creates a list of four functions, one for setting
## the value of the matrix, one for getting the value of the
## matrix, one for setting the value of the inverse, and one for
## getting the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse
## has already been calculated (and the matrix
## has not changed), then cachesolve retrieves
## the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
