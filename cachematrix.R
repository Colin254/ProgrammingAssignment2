## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 2 Functions are used
## to compute the inverse matrix.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix (set)
## get the value of the matrix (get)
## set the value of the inverse of the matrix (setinv)
## get the value of the inverse of the matrix (getinv)

makeCacheMatrix <- function(x = matrix()) {      
	inverse <- NULL
      set <- function(y) {
		x <<- y
            inverse <<- NULL
        }
	get <- function() x
      setinv <- function(inv) inverse <<- inv
      getinv <- function() inverse
      list(set = set, get = get,
		setinv = setinv,
            getinv = getinv)
}

# The cacheSolve function returns the inverse of the matrix. Firstly, the function checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
	inverse <- x$getinv()
      if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
      }
	data <- x$get()
      inverse <- solve(data, ...)
      x$setinv(inverse)
      inverse
}

## For example:
## > x = rbind(c(2, 0, 0), c(1, 0, 2), c(4, 2, 4))
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2] [,3]
## [1,]    2    0    0
## [2,]    1    0    2
## [3,]    4    2    4
## > cacheSolve(m)
##       [,1] [,2] [,3]
## [1,]  0.50  0.0  0.0
## [2,] -0.50 -1.0  0.5
## [3,] -0.25  0.5  0.0
 
