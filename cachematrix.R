### Below are two functions that are used to create a special object that stores a matrix and 
# cache's its inverse.

### makeCacheMatrix creates a special "matrix" object that can cache its inverse. makeCacheMatrix 
# is really a list containing the following functions:
# 1.- set.matrix: set the value of the matrix 
# 2.- get.matrix: get the value of the matrix
# 3.- set.inverse: set the value of the inverse matrix
# 4.- get.inverse: get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv.x <- NULL
        set.matrix <- function(y) {
                x <<- y
                inv.x <<- NULL
        }
        get.matrix <- function() x
        set.inverse <- function(inverse) inv.x <<- inverse
        get.inverse <- function() inv.x
        list(set.matrix = set.matrix, get.matrix = get.matrix,
             set.inverse = set.inverse,
             get.inverse = get.inverse)
}

### The following function calculates the inverse matrix of the matrix stored in the cache (special
# "matrix" object) created with the above function. However, it first checks to see if the inverse 
# matrix has already been calculated. If so (and the matrix has not changed), it gets the inverse 
# matrix from the cache and skips the computation. Otherwise, it calculates the inverse of the 
# matrix stored in the cache and sets the value of the inverse matrix in the cache via the 
# set.inverse function.

cacheSolve <- function(x, ...) {
        inv.x <- x$get.inverse()
        if(!is.null(inv.x)) {
                message("getting cached data")
                return(inv.x)
        }
        data <- x$get.matrix()
        inv.x <- solve(data)
        x$set.inverse(inv.x, ...)
        inv.x
}
