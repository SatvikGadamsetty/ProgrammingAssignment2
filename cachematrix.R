## The functions below cache the inverse of a matrix. The computation of the inverse
## of a square matrix is done via the solve() function. makeCacheMatrix() creates a
## special matrix that can cache its inverse while cacheSolve() calcualtes the inverse
## if it has not been previously calculated. Otherwise, it will retrieve it from the cache.


## This functions creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inv <- function(inverse) m <<- inverse
  get_inv <- function() m
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## This function computes the inverse of the aforementioned special "matrix" object return 
## by the makeCacheMatrix function.If inverse has already been calculated, then cachesolve 
## should retrieve inverse from cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get(m)
  m <- solve(data,...)
  x$set_inv(m)
  m
}
