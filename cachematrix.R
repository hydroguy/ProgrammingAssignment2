# These are a pair of functions that cache the inverse of a matrix.

# The first function creates a special "matrix" object that can cache its
# inverse.  It prepares a list containing four functions that:
# 1) Set the value of the matrix in the parent environment (i.e.,
# the environment from which the function was called).
# 2) Get the value of the matrix in the parent environment (I THINK??)
# 3) Set the value of the inverse matrix in the parent environment
# 4) Get the value of the mean in the parent environment (I THINK??)

makeCacheMatrix <- function(Mat = matrix()) {
  # Mat is a square matrix that is assumed to be invertible
  
  # Determine number of rows in the matrix
  nrows<-nrow(Mat)
  
  # Define a variable to contain the inverse matrix (Note: I attempted to
  # set the value in the upper,left-hand corner of this matrix to NULL, but
  # this was rejected by the compiler.  Instead I used the default NA as a
  # flag for determining if the inverse matrix had been populated yet).
  InvMat<-matrix(nrow=nrows, ncol=nrows)
  
  # Create a function to send the matrix and an "empty" inverse matrix to
  # the parent environment.  "Empty" means a matrix is filled with NA values.
  set <- function(y) {
    # y is the matrix to be sent to the parent environment.  I assumed that
    # it is desirable for the same variable name to be used in the
    # evaluating and parent environments.
    Mat <<- y
    
    # Send an "empty" inverse matrix to the parent environment.  I assumed
    # the number of rows should be re-calculated (but this may not be
    # necessary).
    nrows<-nrow(y)
    InvMat<<-matrix(nrow=nrows, ncol=nrows)
  }
  # Create a function to get the matrix from the parent environment (However,
  # I don't understand why the compiler knows that Mat is in the parent
  # environment, rather than the local environment.)
  get <- function() Mat
  
  # Create a function to set the inverse matrix in the parent environment
  setinverse <- function(y) InvMat <<- y
  
  # Create a function to get the inverse matrix from the parent environment
  # (although, again, why isn't the compiler looking in the local environment
  # for the inverse matrix).
  getinverse <- function() InvMat
  
  # Create a list of the four funtions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix function.  If the inverse was already calculated (and the
# matrix has not changed), then the cacheSolve function retrieves the inverse
# from the cache.
cacheSolve <- function(x, ...) {
  # x is the special "matrix" object created by makeCacheMatrix function.
  
  # Get the inverse matrix from the cache (parent environment)
  InvMat <- x$getinverse()
  
  # If the inverse matrix has already been computed, then return the
  # inverse matrix from the cache.
  if(!is.na(InvMat[1,1])) {
    message("getting cached data")
    return(InvMat)
  }
  
  # Get matrix from the cache (parent environment)
  data <- x$get()
  
  # Compute the inverse of the matrix using the built-in function "solve".
  InvMat <- solve(data)
  
  # Store inverse matrix in the cache (parent environemt)
  x$setinverse(InvMat)
  
  # Return the computed inverse matrix to the calling program
  InvMat
}
