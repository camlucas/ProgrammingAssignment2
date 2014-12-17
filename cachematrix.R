## These functions, together, enable a matrix and it's inverse to be stored together
## The matrix and it's inverse are stored inside the makeCacheMatrix environment,
##   with the makeCachMatrix function only creating a list of four functions in the
##   global environment

## To use these functions, first create/store the matrix inside makeCacheMatrix
##      e.g. x <- makeCacheMatrix(MATRIX)
## The invert/solve the matrix with cacheSolve()
##      e.g. xinv <- cacheSolve(x)
## If the matrix had been previously inverted, cacheSolve() will retrieve the solution
##      - it will only revert to actually solving if no solution had been previously found



## makeCacheMatrix creates the special "matrix", in reality a list of four functions

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(Inverse) I <<- Inverse
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve finds the inverse of a matrix created/stored with makeCacheMatrix
## If previously solved, cacheSolve will recover and return the inverse from cache
## Otherwise, it gets the original matrix, inverts it, 
##    stores the inversion in the makeCacheMatrix environment, and returns the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I
}
