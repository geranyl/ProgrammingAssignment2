## Use the following functions to cache the inverse of
## a given invertible matrix.

## Example use: 
## Create a square matrix m: m<-matrix(c(3,7,0,4), nrow=2, ncol=2)
## Add functions to matrix m that allows for it to be cached: cm<-makeCacheMatrix(m)
## Retrieve the cached matrix's inverse: invCm<-cacheSolve(cm)
## Running cacheSolve on cm again will print the message "getting cached data" and 
## return the already calculated inverse of matrix m.


## makeCacheMatrix: Creates a wrapper of functions for an invertible matrix 
## to allow its inverse to be cached and retrieved without
## calculating the inverse again.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: Returns a matrix that is the inverse of cached matrix 'x' 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
