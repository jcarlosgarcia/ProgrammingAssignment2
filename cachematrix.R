## This code caches the inverse of a matrix. To do that, makeCacheMatrix
## creates a special "matrix" that can cache its inverse, which is passed
## to cacheSolve. This function computes the inverse of the special "matrix"
## if it has not been calculated yet, or retrieves it from the cache otherwise.
## It assumes that the matrix supplied is always invertible
##
## Usage:
## > x <- matrix(c(1,4,2,0,3,1,2,1,0), nrow=3, ncol=3)
## > x
## [,1] [,2] [,3]
## [1,]    1    0    2
## [2,]    4    3    1
## [3,]    2    1    0
## > myMatrix <- makeCacheMatrix(x)
## > cacheSolve(myMatrix)
## [,1] [,2] [,3]
## [1,]  0.2 -0.4  1.2
## [2,] -0.4  0.8 -1.4
## [3,]  0.4  0.2 -0.6
## > cacheSolve(myMatrix)
## getting cached data
## [,1] [,2] [,3]
## [1,]  0.2 -0.4  1.2
## [2,] -0.4  0.8 -1.4
## [3,]  0.4  0.2 -0.6

## This function creates a special "matrix" object that can cache its inverse. It
## creates a list containing functions to get/set the matrix, and to get/set the
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) {
          i <<- inverse
        }
        getInverse <- function() i
  
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated, then it retrieves the inverse from the cache.
## It uses the solve() function to compute the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
