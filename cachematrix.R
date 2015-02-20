makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  get <- function() x
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  getInverse <- function() inverse
  
  setInverse <- function(inv) inverse <<- inv
    
  list(get = get, 
       set = set, 
       getInverse = getInverse, 
       setInverse = setInverse)  
}

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    message("reading from cache")
    return (inverse)
  }
  
  message("computing inverse")
  
  inverse <- solve(x$get())
  x$setInverse(inverse)
  
  inverse
}