## R Programming - week three
## David Hyatt


# in general, use as:
# my_matrix <- makeCacheMatrix(makeTestMatrix())

## The function below makeTestMatrix creates a small matrix
## which is known to be invertable, for testing purposes

makeTestMatrix <- function() {
  # create a small matrix known to be invertible
  myMat<-matrix(c(-0.35,2,2,-0.35), nrow=2, ncol=2)
  myMat
}


# The function makeCacheMatrix creates a special "matrix" object 
# that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# The function cacheSolve computes the inverse of the special "matrix" 
# returned by makeCacheMatrix. If the inverse has already 
# been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("retrieving cached solution")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
