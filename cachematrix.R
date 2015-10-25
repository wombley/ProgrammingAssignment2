## Programming Assignment 2 (Coursera Data Science)

#  Probably not the best way to do this, but I'll go along
#  with it as they're obviously expecting something like
#  their example...


## makeCacheMatrix: Returns essentially a new data type
#                   A matrix that caches it's inverse
#                   This is pretty much a helper function
#                   to cacheSolve (below)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # This is the cached inverse, if available
  # Setter function - if changing value, cached inverse is
  # invalid so blank it
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Simple getter / setters for datatype
  get <- function() x 
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  # Return references to setters/getters
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: Solve (invert) a matrix, caching results

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() #This only works if cached inverse exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Nothing cached, calculate it
  data <- x$get() # Necessary to get environment to cache the thing
  inv <- solve(data, ...) # No error checking, as specified...
  x$setinverse(inv) # Save in cache
  inv # As we actually want to return it, this function should be transparant
}
