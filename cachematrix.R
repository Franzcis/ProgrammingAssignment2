## These functions makeChacheMatrix and CacheSolve help to expedite the
## computation of inverse of a matrix

## makeCacheMatrix amkes a special matrix which is a list containing
## functios to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set<- function (y){
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setinverse <- function (inverse) invert <<- inverse
  getinverse <- function () invert
  list (set = set, get = get,
        setinverse = setinverse, getinverse = getinverse)
}
## cacheSolve calculates the inverse of the matrix created
## if the inverse is already calculated it retreive it from the cache
cacheSolve <- function(x, ...) {
  invert <- x$getinverse()
  if (!is.null(invert)){
    message("getting cached data")
    return (invert)
  }
  newmatrix <- x$get()
  invert <- solve(newmatrix,...)
  x$setinverse(invert)
  invert
}
