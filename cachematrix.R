## This program calculates the inverse of a matrix and stores the result
## in cache. If the inverse function is called again on the same, unaltered 
## matrix, the program retrieves the previously calculated inverse from 
## cache instead of re-calculating.

## The makeCacheMatrix function accepts a square matrix as input parameter and
## converts it to a special object that's inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## The cacheSolve function checks the cache to see if the inverse of the
## matrix x has already been calculated. If so, it retrieves the inverse
## from the cache. If not, it calculates and caches the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
