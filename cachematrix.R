## Put comments here that give an overall description of what your
## functions do
# This file contains the functions necessary to build a matrix wrapper
# that can cache its inverse value (makeCacheMatrix) and a function
# that retrieves the inverse value of such wrapped matrix, or forces
# its computation if a cache is not yet available (cacheSolve). 

## Write a short comment describing this function
# This function builds a wrapper for a matrix object as a list
# that contains functions for retrieving the wrappet value (get),
# resetting it (set), getting a previously stored inverse value
# (getinv) and storing it after computing it for the first time
# (setinv).
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    # Create the set function, to reset the wrapped object to
    # a specific value and delete any cached inverse value.
    set <- function(y) {
        # Use <<- to set the wrapped value by assigning a value
        # in the global environment.
        x <<- y
        # Do the same to reset the cached value.
        i <<- NULL
    }
    # Create a get function that closes around x to return the
    # value of the wrapped matrix.
    get <- function() x
    # Create a get function that sets the cached inverse value
    # by assigning it in the global environment. This is the
    # core of the implementation: setting the value of i in the
    # global environment causes it to be persisted between invocations
    # of getinv, thus acting as a cache.
    setinv <- function(inv) i <<- inv
    # Create a get function that gets the value of the inverse
    # stored in the global environment.
    getinv <- function() i
    # Return a list of the created functions, that acts as a wrapper
    # object through the get function.
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Write a short comment describing this function
# This function computes the inverse of a matrix wrapped in an
# object created by makeCacheMatrix. If the value was previously
# stored, it will return it, else it will calculate it and store
# it before returning it.
cacheSolve <- function(x, ...) {
    # Try to retrieve a value of the inverse from the global
    # environment through the getinv function stored in the list.
    i <- x$getinv()
    if(!is.null(i)) {
        # If the value is present, return it, to save computation time.
        message("Getting the cached inverse value.")
        return(i)
    }
    # Else...
    # Extract the wrapped matrix value.
    data <- x$get()
    # Compute the inverse.
    i <- solve(data, ...)
    # Store the result in the global environment through the
    # setinv function.
    x$setinv(i)
    # Return the computed value.
    i
}