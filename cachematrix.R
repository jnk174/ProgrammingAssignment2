## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cache
  m <- NULL
  
  # Function to set the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  # Function to get the matrix
  get <- function() x

  # Function to set the inverse of a matrix
  setinverse <- function(inverse) m <<- inverse
  
  # Function to get the inverse of a matrix
  getinverse <- function() m
  
  # Return a list of functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  # Check to see if the inverse of matrix has already been calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Function to compute and cache the inverse of the matrix
  data <- x$get()
  m <- solve(data)
  
  ## Return a matrix that is the inverse of 'x'
  x$setinverse(m)
  m
}
