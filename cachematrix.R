## The cachematrix.R file contains two functions, makeCacheMatrix() and cacheSolve(). 
## The first function in the file, makeCacheMatrix() creates an R object that stores a (square) matrix and its inverse.
## The second function, cacheSolve() requires an argument that is returned by makeCacheMatrix() in order to retrieve
## the inverse from the cached value that is stored in the makeCacheMatrix() object's environment.

## Use makeCacheMatrix() to store/cache a matrix and its inverse. Remember that the matrix MUST be square otherwise 
## an error will be displayed!

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        if (nrow(x) != ncol(x)) {print('Error: argument must be a square materix!')}
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve() returns the inverse matrix cached in makeCacheMatrix() environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        if (det(data) != 0) {
                i <- solve(data, ...)
                x$setinverse(i)
        }
        else {print('Error: matrix is not invertible!')}
        i
}
