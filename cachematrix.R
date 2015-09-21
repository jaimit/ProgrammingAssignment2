## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invmatrix <- NULL
    set <- function(y) {
        x <<- y
        invmatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(invm) invmatrix <<- invm
    getinverse <- function() invmatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmatrix1 <- x1$getinverse()
    if(!is.null(invmatrix1)) {
        message("getting cached data")
        return(invmatrix1)
    }
    data <- x1$get()
    invmatrix1 <- solve(data, ...)
    x1$setinverse(invmatrix1)
    invmatrix1
}
