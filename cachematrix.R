makeCacheMatrix <- function(x = matrix()) {
  # Following the same format as the assignment example
  # Creating a makeCacheMatrix object will consist of
  # four functions encapsulated in a list
  # 1. set the matrix
  # 2. get the matrix
  # 3. set the inverse of the matrix
  # 4. get the inverse of the matrix
  
  # Initially set to NULL
  # Changes when the user sets the value
  inv <- NULL
  
  # set function
  # Sets the matrix itself but not the inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get function
  # Gets the matrix itself but not the inverse
  get <- function() x
  
  # Manually set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getinverse <- function() inv
  
  # Encapsulate into a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
}
x <- matrix(1:4, nrow=2, ncol=2)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Following the same format as the assignment example
  
  # Get the current state of the inverse and see if it
  # has been computed yet
  inv <- x$getinverse()
  
  # If it has...
  if(!is.null(inv)) {
    # Simply return the computed inverse		
    message("Getting cached matrix")
    return(inv)
  }
  
  # If it hasn't...
  # Get the matrix itself
  data <- x$get()
  
  # Find the inverse
  inv <- solve(data, ...)
  
  # Cache this result in the object
  x$setinverse(inv)
  
  # Return this new result
  inv    
}