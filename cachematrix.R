## In this assignment, we see how important is to cache the results, 
## in order to use it later, especially for time consuming computations.
## Caching is the art of saving on performance by avoiding computation, using memory,
## and as a data scientist, cashing is very important, useful and in many cases necessary.

## Matrix inversion usually is very costly.


## Function makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x<<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_x <<- inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function just simply computes the inverse of the "matrix"
## returned from the above function called makeCacheMatrix.
## The main reason that solve is called is that it’s a general 
## purpose function that you can use to solve matrix equations without 
## wasting time computing the full inverse, which is often inefficient 
## and time consuming.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
      message ("getting cached inverse matrix")
      return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}
