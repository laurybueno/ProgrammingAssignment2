## The following functions store a square matrix and manages its inverse 
## calculation using cached data when possible


## Prepares a set of internal funcions for assinging and retirieving data 
## corresponding to a matrix and its inverse (when those are available)
makeCacheMatrix <- function(table = matrix()) {
  inverse <- NULL

  set <- function(y) {
    table <<- y
    inverse <<- NULL
  }
  get <- function() table
  setinverse <- function(calculated) inverse <<- calculated
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Receive the set of functions created by makeCacheMatrix and use those 
## to retrieve the original matrix, calculate ints inverse and store it back
## These operations are overrided if a valid inverse is foundin the given set

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
