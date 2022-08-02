## Put comments here that give an overall description of what your
## functions do

##The following two functions will cache the inverse of a matrix.

## Write a short comment describing this function

##The first function can create a new matrix and cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## m is the inverse matrix
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setInverse <- function(Inverse){
    m <<- Inverse
  }
  getInverse <- function() m
  
  list(set = set, get = get, setInverse =setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  if (!is.null(m)) {
    message("getting the inverse matrix")
    return(m)
  }
  x1 <- x$get()
  ## we use solve() to solve x1 * m = identity matrix, i.e, m is the inverse of x1
  
  m <- solve(x1,diag(nrow(x1)))
  x$setInverse(m)
  m
}



