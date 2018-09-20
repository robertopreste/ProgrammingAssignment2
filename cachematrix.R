# makeCacheMatrix and cacheSolve allow to return the inverse of a matrix 
# without needing to recalculate it everytime, but using cached results instead. 

# makeCacheMatrix 
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}


# cacheSolve 
# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated, 
# then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if (!is.null(m)) {
        message("Retrieving cached data...")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
