## The functions below can be used to create a "matrix" object that can cache its inverse,
## for this assignment, we assume that the matrix supplied is always invertible, 
## The functions are similar to the ones explained in the example, though the "mean" 
## function is substitute by inverse function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" object created by 
## makeCacheMatrix function above. It first check to see if the inverse has 
## already been calculated, then it should return the inverse from the cache and 
## skip the computation. Otherwise, it calculates the inverse of the matrix and sets 
## the value in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrixx <- x$get()
  inv <- solve(matrixx, ...)
  x$setInverse(inv)
  inv
}