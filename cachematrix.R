## This script provides functions to
## 1. Create a matrix, calculate its inverse and cache the result.
## 2. Compute the inverse of a matrix or retrieve it from cache if already computed.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # Intitialize the variable that will contain the inverse
    inverse <- NULL
    # Define the general setter function for the matrix
    set <- function(y) {
        x <<- y # Assign y value to x value in the parent environment
        inverse <<- NULL # Clear any value of inverse as if we are setting a new value to the matrix x, the value of its inverse should be re calculated
    }
    # Define the getter function for the matrix
    get <- function() x
    # Define the setter function for the inverse
    setinverse <- function(solve) inverse <<- solve # Assign solve value to inverse value in the parent environment so that setinverse can use it
    # Define de getter for the inverse
    getinverse <- function() inverse
    # Set a list with names so that the different functions can be called by their names in the parent environment, e.g. my_matrix$getinverse
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes/retrieve and return the inverse of the special "matrix" returned
##  by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Try to retrieve the inverse from passed argument
    inverse <- x$getinverse()
    # Check if the inverse is NULL
    if(!is.null(inverse)) {
        # If the inverse is not NULL, it can be returned
        message("getting cached data")
        return(inverse)
    }
    # If the inverse is NULL, we need to calculate it based on the input object
    data <- x$get() # Get the input matrix
    inverse <- solve(data, ...) # Calculate the inverse of the input matrix
    x$setinverse(inverse) # Set the inverse in the input object
    inverse
}
