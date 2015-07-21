## These functions set and retrieve values for an invertible
##  matrix and its inverse.  Note:  Assumes matrix is invertible

## Creates a special vector which returns a list of 
##  functions for:
##    1.  Setting the value of a matrix
##    2.  Getting the value of a matrix
##    3.  Setting the inverse of a matrix
##    4.  Getting the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns the cached inverse of the specified matrix
##  if the inverse has already been generated AND the
##  matrix has not changed.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setinverse(inverse)
  inverse
}
