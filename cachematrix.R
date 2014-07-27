## The following functions create a special object that stores a matrix
## and caches its inverse such that the cached inverse matrix can be  
## returned as long as the original matrix has not changed.

## The makeCacheMatrix function returns a list of functions to set and get
## the value of a matrix and to set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## The cacheSolve function returns the cached inverse of a given matrix
## if it exists.  Otherwise, it calculates, caches, and returns the 
## inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
