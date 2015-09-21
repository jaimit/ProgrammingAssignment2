## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invmatrix <- NULL  ## invmatrix variable caches the inverse of a matirx
    
    ## function set is used to update the value of the input matrix that needs to be inversed
    set <- function(y) {
        x <<- y  
        invmatrix <<- NULL ## in case the set function is called the invmatrix is set back to null
    }
    
    ## function get is used to get the value of the input matirx to the makeCacheMatrix function
    get <- function() x
    
    ## function setinverse is used to set the value of the invmatrix obtained for caching later
    setinverse <- function(invm) invmatrix <<- invm
    
    ## function getinverse is used to get the value of the invmatix cached in the makeCacheMatrix
    getinverse <- function() invmatrix
    
    ## passing all the 4 functions as a output of the makeCacheMatrix 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmatrix1 <- x1$getinverse()
    ## if the inversematrix in cached return its value
    if(!is.null(invmatrix1)) {
        message("getting cached data")
        return(invmatrix1)
    }
    ## if the inversematrix is not chached yet get the matrix and find its inverse
    data <- x1$get()
    invmatrix1 <- solve(data, ...)
    x1$setinverse(invmatrix1)
    invmatrix1
}
