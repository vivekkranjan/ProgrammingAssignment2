## The two functions makeCacheMatrix() and cacheSolve() written below 
## helps in caching the inverse of a matrix. Matrix inversion is usually a costly 
## computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly (e.g. in a loop). 

## Functions Description

## makeCacheMatrix(): This function creates a special "matrix" object that can cache its inverse.

## cacheSolve(): This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the solve function in R. For example,
## if X is a square invertible matrix, then solve(X) returns its inverse.

## For this assignment, assume that the matrix supplied is always invertible.



## Testing the Cache Matrix Function
# m <- matrix(1:4, nrow = 2, ncol = 2)
# x <- makeCacheMatrix(m)
# cacheSolve(x)




## FUNCTION makeCacheMatrix()

# makeCacheMatrix() START 
# This function creates the objects required for makeCacheMatrix with four functions 
# set(),  get(), setinverse() and getinverse()
# 

makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize to NULL, holds the value of inverse later
  inv <- NULL
  
  # Set the value of matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the value of matrix
  get <- function(){
    x
  }
  
  # Assign the value of inverse
  setinverse <- function(inverse){
    inv <<- inverse
  }
  
  # Get the inverse
  getinverse <- function(){
    inv
  }
  
  # Create the list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)	
  
} 

#makeCacheMatrix() END 


## FUNCTION cacheSolve()

# cacheSolve() START 
# This function returns the inverse of matrix

cacheSolve <- function(x, ...) {
  
  # To check whether inverse of matrix is calculated
  inv <- x$getinverse()
  
  # Returns the inverse for matrix that is alrady calculated
  if( !is.null(inv) ) {
    
    # Show message while retreiving cached value
    message("Getting cached matrix....")
    return(inv)
  }
  
  # Else get the original matrix
  data <- x$get()
  
  # Find the inverse of matrix
  inv <- solve(data, ...)
  
  # Cached inverse
  x$setinverse(inv)
  
  # Return the inverse
  return(inv)    
}

# cacheSolve() END 