# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        # Set the matrix
        set <- function(y) {
        x <<- y
        m <<- NULL
        }
        # Get the matrix
        get <- function() {
                x
        }
        # Set the inverse matrix
        setInverse <- function(arg) { 
                m <<- arg
        }
        # Get the inverse matrix
        getInverse <- function() {
                m
        }
        # The result of this function is the list vector, that contains all 
        # functions mentioned earlier
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}



# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse from 
# the cache.
cacheSolve <- function(x, ...) {
        # Use the inverse matrix from the previous function
        m <- x$getInverse()
        # Take the inverse matrix from the cache if we have worked with her 
        # before
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # Get the matrix 
        invMatrix<- x$get()
        # Find the inverse matrix
        m <- solve(invMatrix)
        # Set the inverse matrix to the object
        x$setInverse(m)
        # Print the inverse matrix
        m
}


# x <- matrix(c(1,2,3,4),nrow=2)
# n <- makeCacheMatrix(x)
# n$get()
# cacheSolve(n)
