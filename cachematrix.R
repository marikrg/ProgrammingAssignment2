## Creates a special object that stores a matrix and cache its inverse matrix.
#
# Args:
#   x: One square invertible matrix.
#
# Returns:
#   The special object containing functions to set and get the matrix and
#   functions to set and get the inverse of the matrix.
##
makeCacheMatrix <- function(x = matrix()) {

  cached.inverse.matrix <- NULL
  set <- function(matrix) {
    x <<- matrix
    cached.inverse.matrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse.matrix) cached.inverse.matrix <<- inverse.matrix
  getInverse <- function() cached.inverse.matrix
  
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Caches the inverse of a matrix.
#
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
#
# Args:
#   x: A special object that stores a matrix and cache its inverse matrix.
#
# Returns:
#   The inverse of a given matrix.
##
cacheSolve <- function(x, ...) {
  inverse.matrix <- x$getInverse()
  
  if(!is.null(inverse.matrix)) {
    message("Using cached data")
  } else {
    matrix <- x$get()
    inverse.matrix <- solve(matrix)
    x$setInverse(inverse.matrix)
  }
  inverse.matrix
}
