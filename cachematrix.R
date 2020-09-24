## makeCacheMatrix build a special vector with four functions and cached 
## inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  sol <- NULL
  set <- function(y) {
    x <<- y
    sol <<- NULL
  }
  get <- function() x
  setsolve <- function(msolve) sol <<- msolve
  getsolve <- function() sol
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cache solve resolve inverse matrix using a vector builded with 
## makeCacheMatrix. if inverse is calculated in previous step return 
## the value of cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  sol <- x$getsolve()
  if(!is.null(sol)) {
    message("getting cached data")
    return(sol)
  }
  data <- x$get()
  sol <- solve(data, ...)
  x$setsolve(sol)
  sol
}
