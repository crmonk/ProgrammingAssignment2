## The goal is to invert a matrix, and cache the value to prevent the need to recalculate.
## makeCacheMatrix (given a specific matrix as input) initiates a vector to store the cache.
## cacheSolve can be called many times on a result of a makeCacheMatrix instantiation.
## It only needs to calculate the inverse the first time it is called, thereafter the cache is retrieved.


## This function creates a special "matrix" object that can cache its inverse
## Note that calling the set method will reset the cache.
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


## if you want the inverse of matrix M for the first time you should initiate the special matrix, e.g.: 
##    SpecialM <- makeCacheMatrix(M)
## Then call:
##   cacheSolve(SpecialM)
## The inverse will be calculated if the Matrix is not singular.
## Thereafter, you simply need to call cacheSolve(SpecialM), retrieving the inverse from the cache.
cacheSolve <- function(x,...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
