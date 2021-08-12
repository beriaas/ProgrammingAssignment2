
## Firstly, with makeCacheMatrix function, a special "matrix" object is created 
## that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y){
    x <<- y
    inve <<- NULL
  }
  get <- function(){x}
  setinve <- function(inverse) {inve<<- inverse}
  getinve <- function() {inve}
  list(set=set, get=get, setinve=setinve, getinve=getinve)
}

## with cacheSolve function, the inverse of the special "matrix" returned by the
## makeCacheMatrix is computed. If it already had been calculated then the 
## cacheSolve retrieves the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inve <- x$getinve()
  if(!is.null(inve)){
    message("getting cached data")
    return(inve)
  }
  mat <- x$get()
  inve <- solve(mat, ...)
  x$setinve(inve)
  inve
}
