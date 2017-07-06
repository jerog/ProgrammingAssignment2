## Two functions creating a "cacheMatrix" data type that caches its inverse as determined by the solve() function.

## makeCacheMatrix:
## When passed a regular matrix, returns a list of functions for setting and getting the matrix's content 
## and its inverse. 
## 
## parameters:
## x: the base matrix (NOTE: Assumed to be square and invertible!)
##
## "methods":
## get: returns the base matrix
## set: changes the base matrix content to the matrix y and clears the solve cache.
## getsolve: returns the contents of the solve cache
## setsolve: sets the contents of the solve cache

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solvedMatrix) s <<- solvedMatrix
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve:
## When passed a list of the type created by makeCacheMatrix above, this function checks to see whether x's solve
## cache has content. 
## - If so, it displays that content with the message "getting cached data".
## - If not (solve cache is empty), it uses solve() to compute the inverse of x's base matrix, 
##   sets x's solve cache with that solution, and outputs the solution with no special message.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
