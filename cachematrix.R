## These two functions can give the inverse of a matrix and cache it in a vector.
## If the inverse has already been calculated, then the functions will tell us and
## skip the calculation.
## The function makeCacheMatrix creates an object of type list, with variable sol
## initialized to NULL.It also gives some functions which will be called by the 
## function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  sol <- NULL
  set <- function(y) {
    x <<- y
    sol <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) sol <<- solve
  getsolve <- function() sol
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## If sol is not NULL, then the function cacheSolve skips the calculation and
## return the value of sol. If sol is NULL, the function will calculate the
## inverse of the matrix.

cacheSolve <- function(x, ...) {
  sol <- x$getsolve()
  if(!is.null(sol)) {
    message("getting inversed matrix")
    return(sol)
  }
  data <- x$get()
  sol <- solve(data, ...)
  x$setsolve(sol)
  sol
}
