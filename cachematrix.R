## Testing the Cache Matrix Function
# m <- matrix(1:4, nrow = 2, ncol = 2)
# x <- makeCacheMatrix(m)
# cacheSolve(x)




## FUNCTION makeCacheMatrix()

# makeCacheMatrix() START 
# This function creates the objects required for makeCacheMatrix with four functions 
# set(),  get(), setinverse() and getinverse()

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
  
  # Make the list
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