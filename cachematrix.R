## The following pair of functions (makeCacheMatrix(),
## cacheSolve()) realize the capability of retrieving matrix
## inverse from cache if cached value already exists or 
## calculating the inverse if no cached value exists.

## makeCacheMatrix: 
## creates a special "matrix" object that can cache its inverse.
## The object A contains a list of 4 functions
## - A$set(y): cache the matrix from input (y)
## - A$get(): returns the matrix cached by A$set()
## - A$setinv(invMtx): cache the matrix inverse from input (invMtx)
## - A$getinv(): returns the matrix inverse cached by A$setinv()

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function(y) {
    x <<- y
    xInv <<- NULL 
    ## if the matrix is re-initialized by set(), 
    ## clear the inverse ever cached
  }
  get <- function() x
  setinv <- function(invMtx) xInv <<- invMtx
  getinv <- function() xInv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve:
## computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  xInv <- x$getinv()
  
  ## return cached inverse value if there is one
  if(!is.null(xInv)) {
    message("getting cached matrix inverse")
    return(xInv)
  }
  
  ## calculate the matrix inverse in case of no cached value  
  mtxData <- x$get()
  xInv <- solve(mtxData)
  x$setinv(xInv)
  xInv
}