## Put comments here that give an overall description of what your
## functions do

##The functions makeCacheMatrix and cacheSolve work together to calculate 
##the inverse of a matrix, caching the result rather than compute it repeatedly.
##Please note that they do not work individually,  both functions must be run at the same time
##as each of them depends on the other one.


## Write a short comment describing this function

##The makeCacheMatrix could be considered as kind of a "template" 
##this template creates a List and each element of this list is in fact a function

##set() assign the input argument to the x object in the parent environment and
##set() also assign the value of NULL to the m object in the parent environment. 
##clears any value of m that had been cached by a prior execution of cacheSolve().

##set_inv is the setter for the inverse matrix m
##get_inv is the getter for the inverse matrix m

##the last lines in makeCacheMatrix create the List of functions.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inv <- function(inv) m <<- inv
  get_inv <- function() m
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

## Write a short comment describing this function

##The cacheSolve function is the one that actually calculates the inverse of the matrix
##using the solve funcion that works only for square matrix.
##Please note that the notation x$get is used because we are using the object "get" 
##of the "x" list created in the prior step.

##If the value of m is not equal to NULL, we have a cached inverse matrix and 
##can return it to the parent environment
##cacheSolve() gets the matrix from the input object, calculates the inverse
##Uses the set_inv function to set the inverse of the input matrix
##and returns the value of the inverse to the parent environment by printing the inverse object (m)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inv(m)
  m
}
