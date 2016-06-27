## Put comments here that give an overall description of what your
## functions do
## These functions provide inverse of a matrix. 
## Assumption is that the matrix provided as input to the function
## cacheSolve is an invertible matrix.

## Write a short comment describing this function
## makeCacheMatrix function has four functions to set and get input
## matrix and setinverse/getinverse to set and get inverse of that
## matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {x}
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve function takes an invertible matrix as an input and
## return inverse of that matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- makeCacheMatrix(x)$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- makeCacheMatrix(x)$get()
    m <- solve(data, ...)
    makeCacheMatrix(x)$setinverse(m)
    m
}
