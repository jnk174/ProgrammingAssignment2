## a pair of functions that cache the inverse of a matrix
## Functions to be used as per following example.

## 1. Create a special "matrix" object
## > mat <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))

## 2. Get the matrix
## > mat$get()

## 3. Compute and cache the inverse of the matrix
## > cacheSolve(mat)

## 4. Retrieve the inverse from cache
## > cacheSolve(mat)

## 5. Update the matrix to calculate new inverse
## > mat$set(matrix(c(1, 0, 0, 1), nrow = 2))


## makeCacheMatrix : To creates a special "matrix" object that can cache its inverse.

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


## cacheSolve : To computes the inverse of the special "matrix" returned by 'makeCacheMatrix' above. 
## Before calculate the inverse of matrix , Check to see if the inverse of matrix has already been calculated.

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
