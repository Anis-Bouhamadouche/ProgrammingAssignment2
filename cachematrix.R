## The functions will allow us to create a special matrix and calculate its inverse
## if not available in the cache then cache it for future use without recomputation.

## The makeCacheMatrix function will create a special matrix and can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## The caseSolve will first check if the inverse is computed if not it will 
## compute it and save it to the cache, otherwise it gets it dirrectly from 
## the cache without another computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
