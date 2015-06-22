## The following is a pair of functions that cache and computes the inverse of a matrix
## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y;
      m <<-NULL;
  }
  get <- function() return(x);
  setinver <- function(inver) m <<- inver;
  getinver <- function() return(m);
  return(list(set = set, get = get, setinverse = setinver, getinver = getinver))
}



## This function computes the inverse of the matrix returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getinver()
  if(!is.null(m)) {
    message("Getting cached data...")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinver(m)
  return(m)
}
