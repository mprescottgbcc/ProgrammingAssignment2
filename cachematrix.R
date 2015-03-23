## Programming Assignment 2
## Submitted by: Margaret Prescott
## This script provides functions that maintain cached matrix inverses


## The makeCacheMatrix function creates a new cache of a matrix inverse if one 
## does not already exist
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(trix) {
    x <<- trix
    inv <<- NULL
  }
  
  get <- function() { x }
  
  setInverse <- function(inverse) { inv <<- inverse }
  getInverse <- function() { inv }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}


## The cacheSolve function generates and returns a matrix that is the inverse of 
## a matrix argument passed to the function and stored in the parameter, 'x'
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Retrieving cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

