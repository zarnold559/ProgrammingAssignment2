## These two functions combined provide a convenient method to compute matrix inversion
## through caching the inverse of the matrix. 

## This function creates a special matrix that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
          s <- NULL
          set <- function(y) { ## Sets the values of the input matrix to x
                x <<- y
                s <<- NULL
          }
          get <- function() x ## Gets the values of the matrix 
          setsolve <- function(solve) s <<- solve 
          getsolve <- function() s
          list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## This function computes the inverse of a "special" matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, then this function will retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
          s <- x$getsolve()
          if(!is.null(s)) {
                message("getting cached data")
                return(s)
          }
          data <- x$get()
          s <- solve(data, ...)
          x$setsolve(s)
          s ## Returns the inverse matrix
}
