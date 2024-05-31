# These functions create an object and that can store the inverted matrix, compute
# the inverse, and retrieve the inverse. 

# The first function, makeCacheMatrix, takes a function as input to create a 
# list that contains functions. Specifically, it contains four functions that set
# and get the value of the matrix and set and get the value of the cached matrix.

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invert <<- inverse
  getinverse <- function() invert
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}

# The cacheSolve function takes a cached matrix that checks if the inverse has 
# already been calculated and if not calculates the inverse of the matrix and 
# returns it.

cacheSolve <- function(x, ...) {
  invert <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data)
  x$setinverse(invert)
  invert
}
