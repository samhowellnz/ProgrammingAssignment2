## This set of functions aims to reduce the compute time of repeatadly solving
## a matrix inverse. It does so by caching previously calculated inverse matrix and
## returning that matrix unless it hasn't been calculated before

## This function takes a matrix and then creates the necessary functions for storing
## and returning a cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if an inverse matrix has been created, if so returns that 
## cached matrix, otherwise it calculates the inverse matrix and sends to to the cache
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
