## This function creates a special "matrix" object that can cache its inverse.
##  x is the original matrix
##  set is a function that sets the matrix
##  get is a function that retrieves the matrix
##  setinverse is a function that assigns a value for the inverse of the matrix
##  getinverse is a function that retrieves the inverse of the matrix, if assigned

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.
##  x is the special "matrix" object that can cache its inverse
##  The output is the inverse of the matrix, either from cache if available, or directly calculated


cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}