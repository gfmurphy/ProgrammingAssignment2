## This function creates a special "matrix" object that can cache
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  get <- function() x
  get_inverse <- function() inverse
  set_inverse <- function(inv) {
    inverse <<- inv
  }
  list(get=get, get_inverse=get_inverse, set_inverse=set_inverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above.
##
## If the inverse has already been calculated (and the matrix has not
## changed), then the cachesolve should retrieve the inverse from the
## cache.
cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if (is.null(inv)) {
    inv <- solve(x$get(), ...)
    x$set_inverse(inv)
  }
  inv
}
