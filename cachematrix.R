## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #Given a matrix m, make the matrix able to cache its inverse.
  #Associate the following functions with the matrix:
  # - set(): set the value of the matrix. If the inverse has been calculated,
  #   signal that it must be recalculated. 
  # - get(): get the value of the matrix
  # - setInverse(): set the value of the inverse matrix. Signal that the
  #   inverse does not need to be recalculated. 
  # - getInverse(): if the inverse needs to be recalculated, return NULL. 
  #   Otherwise return the inverse.
  
  recalculate <- TRUE
  i <- NULL
  
  #set the matrix and signal that the inverse should be recalculated
  set <- function(y) {
    x <<- y
    i <<- NULL
    recalculate <<- TRUE
  }
  
  #get the matrix
  get <- function() x
  
  #set the inverse and signal that it does not need to be recalculated
  setInverse <- function(inverse) {
    recalculate <<- FALSE
    i <<- inverse
  }
  
  #get the inverse unless it needs to be calculated, then return NULL
  getInverse <- function() {
    if (recalculate == TRUE) {
      message("matrix changed")
      return(NULL)
    }
    i
  }
  
  #construct the list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  #Given a matrix x, which was generated with the makeCacheMatrix() 
  #function, do the following:
  # - return the inverse of the matrix if it is cached and the matrix 
  #   has not changed since the last time the inverse was calculated
  # - if the inverse is not cached or the matrix has changed since the 
  #   last time the inverse was calculated, calculate the inverse, cache
  #   it, and return it. 
  
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
