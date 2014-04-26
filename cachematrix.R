## Implementation of matrix inversion that caches the computed inverse
## usage: 
##        m = matrix(...)
##        mcached = makeCacheMatrix(m)
##        inverse = mcacheSolve(mcached, ...)
##               calling again mcacheSolve(mcached,...) will not
##               re-compute the inverse, but will return its value from
##               the cache
##
##
##        change the matrix and the get its inverse:
##        mcached$set(newMatrix)
##        inverse = mcacheSolve(mcached, ...) 
##
##        get the value of the cached matrix
##        m = mcached$get()
##
## Example:
##      m = matrix(1:4,nrow = 3,ncol =3)
##      mcached = makeCacheMatrix(m)
##      inv = cacheSolve(mcached)
##      all(solve(m) == inv)

## creates a list with getters and setters for the cached matrix and
## its inverse
makeCacheMatrix <- function(x = matrix()) {
  minv = NULL
  set = function(y){
    x <<- y
    minv <<- NULL
  }
  get = function() x
  setInverse = function(inv) minv <<- inv
  getInverse = function() minv
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}


## returns the cached inverse of the cache matrix
## if the inverse is not present in the cache, the function
## computes it and updates the cache
## the ... parameters are passed to the function 'solve'
cacheSolve <- function(x, ...) {
        
  inv = x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data = x$get()
  inv  = solve(data,...)
  x$setInverse(inv)
  inv
}
