## These functions create a list containing a matrix, the cached copy of its 
## inverse, and four functions that can act on that list. 
## makeCacheMatrix() creates the list, using a normal matrix as input. 
## cacheSolve() takes the list as input, and returns the inverse of the matrix 
## inside. 

## This function creates a cached matrix list using a normal matrix as input. 
## There are four functions that can act on this cached matrix. 
## get() returns the original matrix. 
## set() sets the matrix to the new input. The cached inverse is nullified 
## because it is no longer valid. 
## getinv() returns the cached copy of the inverse. 
## setinv() sets the inverse to the input. 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## This function finds the inverse of the matrix x. 
## If the inverse has already been calculated, it returns the cached copy. 
## Otherwise, it calculates the inverse and stores it in the cache. 
cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
