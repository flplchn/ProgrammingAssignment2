## These functions generate an inverse matrix for every square and invertible matrix that can be stored
## from the local environment where the matrix is computed to the global environment where it can
## be recalled

## The first matrix doesn't compute the actual inversion but initializes the variables instead
## and the functions utilized in the next function

makeCacheMatrix <- function(x = matrix()) {
  inv1 <- NULL
  set <- function(y) {
    x <<- y
    inv1 <<- NULL
  }
  get <- function() x
  setinv <- function(invr) inv1 <<- invr
  getinv <- function() inv1
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function inverts the matrix and caches it for the global environment

cacheSolve <- function(x, ...) {
  inv1 <-x$getinv()
  if(!is.null(inv1)) {
    message("getting cached data")
    return(inv1)
  }
  data <- x$get()
  inv1 <- solve(data,)
  x$setinv(inv1)
  inv1
        ## Return a matrix that is the inverse of 'x'
}
