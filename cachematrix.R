## makeCacheMatrix function returns a list of 4 functions
## cacheSolve calculates the inverse of matrix if it has not been calculated
## and caches it
## If the inverse is already cached, cacheSolve function just returns it

## returns a list of operations, including set the value of matrix, get the
## value of matrix, calculate the inverse, get the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve(x)
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## return the inverse if it is cached
## if called for the first time, use solve to calculate the inverse
## cache it, then return the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  return(m)
}
