## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix rather than compute it 
## Following functions are used to Cache and Inverse the matrix

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize inverse matrix 
  i <- NULL
  
  ## set the original matrix and reset inverse matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## get the original matrix
  get <- function() x
  
  ## set inverse values
  setinv <- function(inv) i <<- inv
  
  ## get inversed values 
  getinv <- function() i
  
  ## list of all special matrix functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## get the inverse from cache
  i <- x$getinv()
  
  ## check if the value exists in cache and return if exists
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## if the value is not cache, get the matrix value
  data <- x$get()
  
  ## Compute inverse using solve function
  i <- solve(data, ...)
  
  ## set the inverse value
  x$setinv(i)
  ## Return a matrix that is the inverse of 'x'
  
  i
}
