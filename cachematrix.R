## This program code gets the inverse of a matrix. This attempts to use cached version of a matrix
## inverse, if that is available

## This function takes a numeric vectors as input.
## It creates a vector of 4 functions set, get, setinv (for setinverse) and getinv (for getinverse)
## These functions will be used in the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinv <- function(m) minv <<- m
  getinv <- function() minv
  list(set = set, get = get,setinv = setinv,getinv = getinv)

}



## This function takes the vector produced by makeCacheMatrix as input
## It checks if the inverse of the matrix passed to makeCacheMatrix is in cache. If yes it gets it from cache
## If no, it computes it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  minv <- solve (data)
  x$setinv(minv)
  minv
}
