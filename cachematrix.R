## Put comments here that give an overall description of what your
## functions do
## The following functions compute the inverse of a matrix and then store it in cache 
## if it is needed so that it is not calculated eyery time it is needed

## Write a short comment describing this function
## The following function create a special matrix object which can be used by the 
## cacheSolve function to calculate and store the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## The following function take an object of makeCachMatrix and compute its inverse
## if it is not computed already if computed then it returnes the cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
