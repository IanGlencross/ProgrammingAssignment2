## These two functions create a matrix with a cached inverse.
## To create a new matirx from an existing matrix use 
##   newMatrixName <- makeCacheMatrix(exitsingMatrixName)
## You can declare an empty cached matrix using
##   newMatrixName <- makeCacheMatrix()
## You can set the values of the matrix by
##   MatrxName$set(matrixValues)
## To access the current value of the matrix you must use
##   MatrixName$get()
## To find the inverse of the Matirix use
##   cacheSolve(MatrixName)
## This is either calculate eth inverse (if it exists) or 
## return the inverse from the cached value.
##
## The functions  setinv and getinv are not designed to be
## accessed directly by the user.


## The makeCacheMatrix function take a maxtrix as an input
## and creates a environment where the values of both
## the mtrix and it's inverse can be stored and also
## a list of methods for updating (set) and accessing (get)
## both the matirix and it's inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function is used as a replacement of the solve
## function that accesses a previously calculated value for the 
## inverse of a matrix rather than calculate it each time.
## Matrices that have not been inverted yet get inverted using
## the standard solve function and then it saves this value.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
