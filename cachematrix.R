##  Programming Assignment 2: Caching the Inverse of a Matrix
#     This functions let you calculate the inverse of a matrix, caching the result
#     for later use.
#
#   Example:
#     m <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
#     cacheSolve(m)


makeCacheMatrix <- function(x = matrix()) {
  # Creates a special matrix oject that can cache its inverse
  #
  # Args: 
  #   x: matrix
  #
  # Returns:
  #   A list containing the following methods:
  #     set, to set the value of the matrix
  #     get, to get the value of the matrix
  #     setInverse, to set the cached value for the inverse
  #     getInverse, to get the cached value for the inverse
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cache <<- inverse
  getinverse <- function() cache
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}



cacheSolve <- function(x) {
  ## Calculates the mean of a special "matrix" created with the function makeCacheMatrix
  #
  # Args: 
  #     x: matrix created with the function makeCacheMatrix
  #
  # Returns:
  #     The inverse of the matrix
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}