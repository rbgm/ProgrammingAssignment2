## Put comments here that give an overall description of what your
## functions do

## Pair of functions that cache the inverse of a matrix


## Write a short comment describing this function

## This function creates a special "matrix" object (as a list) that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
## For this assignment, it's assumed that the matrix supplied is always 
## invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


## Example calls
#a <- matrix(1:4, nrow = 2, ncol = 2)
#b <- makeCacheMatrix(a)
#cacheSolve(b)
## Calling it again it retrieves cached data
#cacheSolve(b)



