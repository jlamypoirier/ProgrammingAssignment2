#The functions "makeCacheMatrix" and "cacheSolve" compute the inverse 
#of a matrix, and use caching to store the results of the computation for
#future use


#"makeCacheMatrix" creates a placeholder for a matrix and its cached inverse, 
#as well as functions to get and set them.
#It returns a list containing the four access function
#"set", "get", "getinv", "setinv"
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       getinv = getinv,
       setinv = setinv)
}

#"cacheSolve" takes an object created by "makeCacheMatrix",
#and returns the cached inverse if it exists,
#otherwise it computes it and stores it in cache, then returns the result.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    #message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
