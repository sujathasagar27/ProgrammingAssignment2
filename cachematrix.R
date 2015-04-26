## The function makeCacheMatrix returns a matrix by creating a cache that is used by cacheSolve.
## It returns a list containing functions to set and get the matrix, set and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## This function cacheSolve returns inverse of the Matrix that was input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  ## Below if condition is to check if the inverse is already calculated if so, 
  ## get the data from the cache and skip the computation.
  if(!is.null(inv)) {
    message("This print statement is to know that the data is pulled from the cache and not computed.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
