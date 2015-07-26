## These two function enable caching the inverse of a matrix submitted to them

##  This function creates a special "matrix" object that can cache its inverse.
##  Assumes that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated the cached version is retrieved, if not, it creates the inverse and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      m <- x$getInverse()
  
      if(!is.null(m)) {
          
        message("getting cached data!")
        return(m)
      }
  
      data <- x$get()
      m <- solve(data)
      x$setInverse(m)
      m
  
}
