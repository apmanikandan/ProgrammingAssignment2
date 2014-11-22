# makeCacheMatrix creates a list containing a function to create a special "matrix" object that can cache its inverse - follow the example for vector in assignment.
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,setInverse = setInverse,
             getInverse = getInverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
        
## Sample
## x <- matrix(1:4,2,2)
## > m = makeCacheMatrix(x)
## > m
## $set
## function (y) 
## {
##         x <<- y
##         m <<- NULL
## }
## <environment: 0x11f8c76a8>
##         
##         $get
## function () 
##         x
## <environment: 0x11f8c76a8>
##         
##         $setInverse
## function (inverse) 
##         inv <<- inverse
## <environment: 0x11f8c76a8>
##         
##         $getInverse
## function () 
##         inv
## <environment: 0x11f8c76a8>
##         
## > m$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(m)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)
## getting cached data.
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(m)
## getting cached data.
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 