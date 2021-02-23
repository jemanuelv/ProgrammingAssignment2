# Implement a pair of functions that cache the inverse of a matrix

# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set_x <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get_x <- function() x
  set_xinv <- function(y) xinv <<- y
  get_xinv <- function() xinv
  list(set_x = set_x, get_x = get_x, set_xinv = set_xinv, get_xinv = get_xinv)

}

# Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated then retrieve the inverse from 
# the cache.

cacheSolve <- function(x, ...) {
  
  xinv <- x$get_xinv()
  
  if(!is.null(xinv)) {
    return(xinv)
  }
  
  data <- x$get_x()
  xinv <- solve(data, ...)
  x$set_xinv(xinv)
  
  xinv
}
