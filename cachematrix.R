# Calculation of Matrix inversion usually consumes lot of time and hence it is always wise
# to store the cache of the inverse of a matrix rather than computing it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


# The following function returns the inverse of the matrix. It will check, if
# the inverse has already been computed. If so, it gets the result from cache and will not
# computate the inverse one again. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the input matrix is always a square matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
