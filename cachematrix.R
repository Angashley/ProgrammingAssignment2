## Caching the inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## Given an invertible matrix, the following pair of functions will calculate the inverse matrix or retrieve the inverse matrix from the cache if the inverse has already been calculated. 

## The makeCacheMatrix function creates a matrix object that can cache its inverse.
## makeCacheMatrix creates a list containing a function to -:
## a) set the value of the matrix
## b) get the value of the matrix
## c) set the value of its inverse
## d) get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list (set = set, get = get, 
            setinverse = setinverse,
            getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix above. 
## Firstof all, it checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skip the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
