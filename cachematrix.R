## Put comments here that give an overall description of what your
## functions do
## Overall: Below are a pair of functions that cache and compute the inverse of a matrix.

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y;
    inverse <<- NULL
  }
  get <- function() return(x);
  setinverse <- function(inv) inverse <<- inv;
  getinverse <- function() return(inverse);
  return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not chnaged),
## then the cacheSolve should retrive the inverse from the cache.


cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  return(inverse)
}

