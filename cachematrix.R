## As there may be some benefit to cache the inverse of a matrix rather than computing it repeatly
## which is usually a costly computation.
## These two functions below are created to cache the inverse of a matrix. 

## This function creates a special "matrix" object that can cache its inverse
## The function makeCacheMatrix creates a special "vector", which is really a list contain
## a function to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## The following function cacheSolve computes the mean of the special "vector" created with
## the function above.
## It firstly checks if the inversion has already been computed.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it computes the inversion of the matrix and sets the value in the cache via the 
## setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()  
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
