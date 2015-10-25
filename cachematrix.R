## Like the example for makeVector, a function is defined and 
## assigned to makeCacheMatrix where x is a matrix

## A matrix is created which allows the saving of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  flip <- NULL
  set <- function(y) {
    x <<- y
    flip <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) flip <<- inverse
  getInverse <- function() flip
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function can calculate the inverse of a matrix
## unless it finds that the same matrix inverse has already been calculated.

cacheSolve <- function(x, ...) {
  
  flip <- x$getInverse()
  if (!is.null(flip)) {
    message("getting cached data")
    return(flip)
  }
  mat <- x$get()
  flip <- solve(mat, ...)
  x$setInverse(flip)
  flip
}