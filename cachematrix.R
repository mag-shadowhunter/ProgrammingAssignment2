## Caching the Inverse of a Matrix

## This below function can create a "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()){
  inve1 <- NULL
  set <- function(y) {
    x <<- y
    inve1 <<- NULL
  }
  
  get <- function() x
  setInv <- function(inverse) inve1 <<- inverse
  getInv <- function() inve1
  
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## This section function computes the inverse of the "matrix" created by above makeCacheMatrix

## If matrix is not changed & inverse is calculated then the inverse would be extracted from the cache.

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  
  inve1 <- x$getInverse()
  
  if (!is.null(inve1)) {
    message("getting cached data")
    return(inve1)
  }
  
  mat <- x$get()
  
  inve1 <- solve(mat, ...)
  x$setInverse(inve1)
  inve1
  
  }
