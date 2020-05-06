## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  y <- NULL
  set <- function(z) {
    x <<- z
    y <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse) {
    y <<- inverse
  }
  getInverse <- function() {
    y
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  y <- x$getInverse()
  if (!is.null(y)) {
    message("getting cached data")
    return(y)
  }
  data <- x$get()
  y <- solve(data, ...)
  x$setInverse(y)
  y
}
