## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly
##
##
## The purpose of these functions is to write a pair of functions that
## cache the inverse of a matrix.








## This function creates a special matrix-associated object that can cache the matrix's inverse.
## The cached inverse is NULL upon creation
## The inverse is calculated/cached using the following function, 'cacheSolve'
##
## Argument x: a square matrix (assume it is invertible)
## Returns: a list of functions acting on the said matrix and its cacheable inverse
##
##
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}











##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
##
## Argument x    :  list of functions acting on a matrix and its cached inverse,
##                  created from preceding function 'makeCacheMatrix(x = matrix())'
##          ...  :  extra arguments to be passed to the 'solve(matrix, ...)' method
##
## Returns       :  a matrix that is the inverse of 'x'
##
##
##
##
##
##
##
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}







