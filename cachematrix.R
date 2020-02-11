## The first function, makeCacheMatrix, receives an invertible matrix and defines a list of 
## functions to get the value of the matrix, set the inverse of the matrix and get the 
## inverse of the matrix. The second function calculates the inverse of the matrix if it
## has not been calculated before, otherwise it returns the cached value

## makecacheMatrix receives a matrix and defines a list of functions that will enable
## storing and retrieving the inverted matrix 

makeCacheMatrix <- function(x = matrix()) {
     m <<- NULL
     get <- function () x
     setinverse <- function (inverse) m <<- inverse
     getinverse <- function () m
     list(get=get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## cacheSolve inverts the "matrix" created by makeCacheMatrix. If the value has already
## been calculated then the value is retrieved from the cache 
cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data)
     x$setinverse(m)
     m
}
