
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix

## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  }
  
  get <- function() x
  setinvert <- function(invert) m <<- invert
  getinvert <- function() m
  list(set = set, get = get,
    setinvert = setinvert,
    getinvert = getinvert)
  
}


## This will compute the inverse of the matrix created in makeCacheMatrix
## If the inverse has already been computed, it will retrive the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinvert()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvert(m)
  m
        
}
