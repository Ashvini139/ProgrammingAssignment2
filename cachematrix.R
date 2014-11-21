## Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation.
# Their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
# Creates a special "Matrix", which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# This function calulates the inverse of the special "Matrix" created with the above function.
# However, it first checks to see if the inverse is calculated. 
# if so it gets the inverse from the cache and skips the computation. 
# Otherwise, it calcualtes the inverse of the data and sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
