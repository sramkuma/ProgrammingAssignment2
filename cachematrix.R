## The 2 functions below cache the inverse of a matrix, and compute the inverse, if necessary

## This function creates a special "matrix" object that can cache its inverse. It is basically a list containing
## 4 functions -1) set the value of the matrix, 2) get the value of the matrix, 3) set the value of the inverse
## and 4) get the value of the inverse

makeCacheMatrix <- function(x = numeric()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  }
  get <- function() x
  
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse had already been computed, and the 
## matrix hasn't changed, then the function retrieves inverse from the cache

cacheSolve <- function(x,...)  {
  
  m <- x$getInverse()
  if(!is.null(m))  {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}