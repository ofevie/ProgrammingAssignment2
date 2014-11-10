## (Put comments here that give an overall description of what your
## functions do)

## The idea behind these functions is to save computation time 
## by Caching the inverse of a matrix.
## If the contents of the matrix are not changing, it makes sense
## to cache the inverse matrix so that when we need it again
## we can look it up in the cache rather than recomputing it.
## The input matrix is to be square and invertible.



## (Write a short comment describing this function)
## makeCacheMatrix creates a special "matrix" that can cache its inverse
## It is a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
      inv <- NULL
      set <- function(y) {
            m <<- y
            inv <<- NULL
      }
      get <- function() m
      setinverse <- function(solve) inv <<- solve
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## (Write a short comment describing this function)
## The input of this function is the output of the function makeCacheMatrix.
## cacheSolve calculates the inverse of the special "matrix" 
## created with makeCacheMatrix.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value 
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      ## Return a matrix that is the inverse of 'x'
      inv
}
