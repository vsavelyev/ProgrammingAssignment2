# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(mean) { 
    m <<- mean
  }
  getInverse <- function() m
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse from 
# the cache.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  invMatrix<- x$get()
  m <- solve(invMatrix)
  x$setInverse(m)
  m
}


# x <- matrix(c(1,2,3,4),nrow=2)
# n <- makeCacheMatrix(x)
# n$get()
# cacheSolve(n)
