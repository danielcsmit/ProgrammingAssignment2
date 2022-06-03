## Represents a matrix and a cached inverse of that matrix
makeCacheMatrix <- function(myMatrix = matrix()) {
  inverse <- NULL
  
  setMatrix <- function(newMatrix) {
    myMatrix <<- newMatrix
    inverse <<- NULL
  }
  getMatrix <- function() myMatrix
  setInverse <- function(newInverse) inverse <<- newInverse
  getInverse <- function() inverse
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Return a matrix that is the inverse of 'cacheMatrix',
## using the cached value if it exists
cacheSolve <- function(cacheMatrix, ...) {
  inverse <- cacheMatrix$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  myMatrix <- cacheMatrix$getMatrix()
  inverse <- solve(myMatrix, ...)
  
  cacheMatrix$setInverse(inverse)
  
  inverse
}
