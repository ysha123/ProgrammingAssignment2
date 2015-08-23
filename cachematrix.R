## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  x <- NULL 
  set <- function(y) {
    x <<- y 
    inv_x <<- NULL
  }
  get <- function() x
  set_inv <- function(solve) inv_x <<- solve(x)
  get_inv <- function() inv_x
  list(set = set, get = get,
       set_inv = set_inv, 
       get_inv = get_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$get_inv()
  if(!is.null(inv_x)) {
    message("Retrieving value for inv_x")
    return(inv_x)
  }
  data <- x$get
  x <- solve(data, ...)
  x$set_inv(inv_x)
  inv_x
}
