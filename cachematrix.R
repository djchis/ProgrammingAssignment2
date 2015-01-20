## Put comments here that give an overall description of what your
## functions do
# functions create a cache of a matirx and its inverse to skip calculation if already done.

## Write a short comment describing this function
# function below creates a list of functions to take in a matrix and keep track of whether the inverse has been found


makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv) 
    
}


## Write a short comment describing this function
# The following function calculates the inverse of the special "matrix" created with the above function.
# However, it first checks to see if the mean has already been calculated. If so, it gets the inverse from the cache.
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
