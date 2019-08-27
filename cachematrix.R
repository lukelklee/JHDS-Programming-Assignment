## This module includes 2 functions that can create a special object that stores
## an invertible matrix and caches its inverse.

## This function creates a special "matrix" which sets the invertible matrix
## and its calculated inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(matinv) inv <<- matinv
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## The following function calculates the inverse of the special 
## "matrix" created with the makeCacheMatrix function above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}

