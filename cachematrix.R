## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      ## initialize and set the value of the matrix
      mx<- NULL
      set <- function(y) {
            x <<- y
            mx <<- NULL
          }

      ## get the matrix value
      get <- function() { x }
     
      ## set the inverse of the matrix
      setinverse <- function(solve) { mx <<- solve }
      getinverse <- function() { mx }
      
      ## get the inverse of the matrix
      list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
    }
 
## Compute the inverse of the special matrix returned by "makeCacheMatrix" 
## above. If the inverse has already been calculated (and the matrix has not 
## changed), then the "cachesolve" should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
      ## Returns a matrix inverse of 'x'
      
      ## get the inverse of the matrix        
      mx <- x$getinverse()
      
      ## if the matrix is currently available then return it
      if(!is.null(mx)) {
        message("getting cached data")
            return(mx)
          }
      ## get the matrix object and calc the inverse
      data <- x$get()
      mx <- solve(data, ...)
      
      ## set the inverse of the matrix 
      x$setinverse(mx)

      ## return
      mx
    }
