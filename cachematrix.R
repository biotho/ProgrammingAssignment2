## Below are two functions that are used to create a special object that stores  
## an invertible matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a list containing a function to
## 1.set the matrix value, 2.get the matrix value 
## 3.set the inverse matrix value,  4.get the inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        
      list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## The following function calculates the inverse of a matrix x. It first checks
## to see if the inverse has already been calculated, and returns the inverse
## from the cache if so and skips the computation. Otherwise it calculates the
## inverse of the matrix and sets the value of the inverse in the cache via the 
## setinv function.

cacheSolve <- function(x, ...) {
 
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
