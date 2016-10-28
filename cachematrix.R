#Assignment 2 R intro course

## Put comments here that give an overall description of what your
## functions do
## Below are two functions: makeCacheMatrix creates a matrix which is able to cache it's inverse.
## Cachesolve returns the inverse of the output of the first function. 

## Write a short comment describing this function: makeCacheMatrix needs an invertible matrix. 
## it creates a special matrix which caches it's inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function:
## cacheSolve calculates and returns makeCacheMatrix's inverse 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}

#Example

i<-matrix(c(1,2,3,4), nrow=2, ncol=2)

d<-makeCacheMatrix(i)

cacheSolve(d)


