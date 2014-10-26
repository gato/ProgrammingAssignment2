## Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly 
## These functions create a matrix that can cache its inverse
## 
## use example:
## 
## a = matrix(1:4,2,2)
## m = makeCacheMatrix(a)
## i = cacheSolve(m)
## i2 = cacheSolve(m)
## will print message "getting cached data" as computation was not needed
## 

##############################################
##
## makeCacheMatrix(x = matrix())
##
## parameters:
##   x: matrix to be inverted (it's expected to be inversible)
##
## returns: an special matrix that contains the matrix passed as parameter
##          and can cache it's inverse
## 
makeCacheMatrix <- function(x = matrix()) {
  ## inverse start in non-computed state
  inverse <- NULL
  set <- function(y) {
    x <<- y
    ## changing the contained matrix clears the inverse to force a recalculation
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##############################################
##
## cacheSolve(x, ...)
##
## parameters:
##     x: matrix created with makeCacheMatrix
##   ...: other parameters that can optionaly be passed to solve
##        check solve documentacion with ?solve for more information
##      
## returns: the inverse of matrix x
## 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    ## if x has the inverse already computed return it
    message("getting cached data")
    return(inverse)
  }
  ## inverse is not computed
  ## get the original matrix
  data <- x$get()
  ## compute the inverse
  inverse <- solve(data, ...)
  ## store inverse for future use
  x$setinverse(inverse)
  inverse
}
