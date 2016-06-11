## Provides functions for custom matrix which caches the inverse of a matrix upon first calculation

## Creates a special matrix which can cache a single value for the matrix, also provides the setter and 
## for the special matrix

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y) {
      x <<- y
      v <<- NULL
    }
    
    get <- function() x
    
    setCacheValue <- function(value) v <<- value
    getCacheValue <- function() v
    
    list(set=set, get=get, setCacheValue=setCacheValue, getCacheValue=getCacheValue)
}


## Finds the inverse of the special matrix and cache its value using cache functions of the special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getCacheValue()
    
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    
    x$setCacheValue(i)
    x
}
