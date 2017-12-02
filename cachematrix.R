## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly.
## Here is a pair of functions that cache the inverse of a matrix:

## 1.
## This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  s <- NULL
  
  ## Set the matrix
  set <- function(y) {
    m <<- y
    s <<- NULL
  }
  
  ## Get the matrix
  get <- function() m
  
  ## Set the inverse of the matrix
  setinverse <- function(solve) s <<- solve
  
  ## Get the inverse of the matrix
  getinverse <- function() s
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 2.
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(m, ...) {
        
  s <- m$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- m$get()
  s <- solve(data, ...)
  m$setinverse(s)
  s
}
