## The set of functions in this file provide a way to calculate the inverse of a matrix
## and cache the result to help speed up the calculations when these are repeated.

## makeCacheMatrix creates a special vector listing 4 functions to set and get the
## matrix from cache and setinv and getinv to store and retrieve the inverse of
## the matrix from cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv # store the inverse in cache
  getinv <- function() m  # this is used by cacheSolve to check if the inverse is available in cache
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

##cacheSolve is a function to calculate the inverse of the given matrix. This function
## checks the cache and if the result of a previous run is available that is fectched
## and returned, else, the inverse is calculated afresh and stored in cache before returning

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()   # Check if the inverse is availble in cache
  if(!is.null(m)) {  # if the inverse is available, that is returned in the next step
    message("getting cached data")
    return(m)
  }
  data <- x$get()    # if the inverse is not available in cache,
  m <- solve(data)   # the inverse is calculated
  x$setinv(m)        # and stored in cache, before returning the same.
  m

}

