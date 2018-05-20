## Creates a Matrix Object and caches the inverse/calculate the inverse if it has not already been calculated

## This function creates a Matrix Object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
        x <<- y
        i <<- NULL
      }
      
      get <- function() x
      setInv <- function(inv) i <<- inv
      getInv <- function() i
      
      list(set = set, get = get, 
           setInv = setInv, 
           getInv = getInv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInv()
    if(!is.null(i)) {
      return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInv(i)
    i
}
