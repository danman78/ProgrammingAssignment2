## These functions take a matrix and build a series of functions around it that allow data about the matrix to be stored.
## makeCacheMatrix takes a matrix as an input (assuming it is a n by n matrix) and builds functions for calling
## the matrix as well as the inverse.
## cacheSolve evaluates the matrix object created by the first function.  It first checks to see if the matrix
## object has already computed an inverse, if so it returns the inverse.  If not, it calculates the inverse
## and stores the inverse in the matrix object.  It then returns the inverse.

## Creates matrix object

makeCacheMatrix <- function(x = matrix()) {
  
  inverse  <- NULL
  set <- function(y){
    x <<- y
    inverse <<-NULL
  }
  get <- function() x
  setinverse <- function(solve)  inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}

## Checks matrix object for existing inverse, calculates one if needed.

cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return (inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}