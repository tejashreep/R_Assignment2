## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Here,makeCacheMatrix creates a list containing a function to
# 1. set and get the value of the matrix
# 2. set and get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# Here cacheSolve function returns the inverse of the matrix. It does the following:-
# 1. It first checks whether the inverse has already been computed.
# 2. If so, it gets the result and skips the computation.
# 3. If not, it computes the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv) 
  inv
}
