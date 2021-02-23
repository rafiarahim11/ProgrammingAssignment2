##There are 2 functions makeCacheMatrix() and cacheSolve().
## Both functions together can be used to cache the inverse of a matrix 
## and, if the inverse is already in the cache, retrieve it from there and return it.

## The function makeCacheMatrix() returns a list, containing functions.
## These functions get/set a matrix object in the environment of makeCacheMatrix
## And they set/get inverse of that matrix object.

makeCacheMatrix <- function(x = matrix()) {
   
    inv <- NULL     #initializing inverse as NULL
    set <- function(y) {
          x <<- y
          inv <<- NULL
      
    }
    get <- function() x   #get the value of matrix x
    setInverse <- function(inverse) {inv <<- inverse} #now set the value of an inverse
    getInverse <- function() inv #now get the value of inverse
    list(set = set,
         get = get, getInverse = getInverse,
         setInverse = setInverse)
    
  }




## ## The function cacheSolve takes arguments of the type makeCacheMatrix.
cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
  }
    
  
