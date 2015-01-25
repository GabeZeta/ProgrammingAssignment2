# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than compute it repeatedly.
# The following two functions are used to cache the inverse of a matrix.

# The function makeCacheMatrix() creates a special "matrix" object that 
# can cache its inverse.It's really a list containing a function to
#   1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the value of inverse of the matrix
#   4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
  {
    m <- NULL                     # default if cacheSolve hasn't yet been used
    set <- function(y) 
      {
        x <<- y                   # caches the inputted matrix so that 
        m <<- NULL                # cacheSolve can check whether it has changed
      }
    get <- function() x           # obtain "raw" matrix
    setinverse <- function(inverse) m <<- inverse     # assign computed inverse matrix (of x) to m
    getinverse <- function() m                        # obtain the cached inverse matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }


# The function cacheSolve() returns the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache and skip the computation.If not, it computes the 
# inverse, sets the value in the cache via the setinverse() function.

# This function assumes that the matrix supplied is always invertible.


cacheSolve <- function(x, ...) 
  {
    # Return a matrix that is the inverse of 'x'
  
    invMat <- x$getinverse()   # check if the inverse has already been calculated
    if(!is.null(invMat))       
      {
        message("Cached data found. Getting cached data... Done!")  # if so, get the inverse from the cache
        return(invMat)
      }
    message("No cached data found. Calculating inverse matrix...")
    data <- x$get()            
    invMat <- solve(data)      # if not, calculate the inverse and
    x$setinverse(invMat)       # set the value in the cache via setinverse()
    message("Done!")
    invMat
  }
