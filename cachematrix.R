## Two functions makeCacheMatrix and cacheSolve are used
## makeCacheMatrix is used to define a function to set the 
## matrix using "set" and "get" to return the matrix
## setinverse is used to set the inverse of m to inverse
## and getinverse return the inverse matrix, m
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## this function utilizes another builtin function "is.null()"
## solve() is used to calulate the inverse of the matrix
## if m is null, inverse is calulate using solve()
## if it is previously calulated, cached value is returned. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
      
      }
