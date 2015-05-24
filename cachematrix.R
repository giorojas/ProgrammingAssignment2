## Put comments here that give an overall description of what your
## functions do

## This function gets the inverse of a matrix
## First it sets the value of the matrix, then gets the values of the matrix
## Sets the value of the Inverse and gets the values of the Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function () m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
    
}


## This function computes the inverse of the 
## matrix returned by makeCacheMatrix() by either calculating it
## or retrieving it from cache if it has been done before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
