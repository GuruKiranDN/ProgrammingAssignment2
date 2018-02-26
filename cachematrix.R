# makeCacheMatrix is a function that returns a list of functions.Its main puspose is to store a martix and a cached value of the inverse of the matrix.
makeCacheMatrix <- function(x = numeric()) {
  
  # Stores the cached value or NULL if nothing is cached
  # initially nothing is cached so set the cache vector to NULL
  cache <- NULL
  
  # store a matrix in a vector called setmatrix
  setMatrix <- function(newValue) {
    x <<- newValue
    # cache has to be cleared because its an new variable and no value is stored
    cache <<- NULL
  }
  
  # to return the matrix the following is typed 
  getMatrix <- function() {
    x
  }
  
  # the given value is cached 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # the cache value is stored in the variable 
  getInverse <- function() {
    cache
  }
  
  # the list is returned.
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# this function calculates the inverse of the matrix
cacheSolve <- function(y, ...) {
  #cached value is stored
  inverse <- y$getInverse()
  # the cache value will be returned if it is present
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # else calculate the values and store it in cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  # the value of the cache is returned 
  inverse
}