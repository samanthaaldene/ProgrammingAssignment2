## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
##Your assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_ <- NULL
        set <- function(y) {
                x <<- y
                inv_ <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv_ <<- inverse
        getInverse <- function() inv_
        list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_ <- x$getInverse()
        if(!is.null(inv_)) {
                message("getting cached data")
                return(inv_)
        }
        data <- x$get()
        inv_ <- solve(data, ...)
        x$setInverse(inv_)
        inv_
}
