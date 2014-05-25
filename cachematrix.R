## R Programming - Programming Assignment 2: Lexical Scoping
## A pair of functions that cache the inverse of a matrix.
## Assumes that the matrix supplied is always invertible.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Usage example: m <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))

makeCacheMatrix <- function(x = matrix()) {
        # Creates a special "matrix" object that can cache its inverse.
        #
        # Args:
        #   x: A square invertible matrix
        #
        # Returns:
        #   A special "matrix", which is really a list containing a function to:
        #     1. set the value of the matrix : set
        #     2. get the value of the matrix : get
        #     3. set the value of the inverse of the matrix : setinverse
        #     4. get the value of the inverse of the matrix : getinverse
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve will instead retrieve the inverse from the cache.
## Usage example: cacheSolve(m)

cacheSolve <- function(x, ...) {
        # Computes the inverse of the "matrix" returned by makeCacheMatrix.
        # If the inverse has already been calculated (and the matrix has not 
        # changed), then it will instead retrieve the inverse from the cache.
        #
        # Args:
        #   x: The special "matrix" object returned from makeCacheMatrix
        #
        # Returns:
        #   A matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
