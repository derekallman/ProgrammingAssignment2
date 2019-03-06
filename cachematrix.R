## Put comments here that give an overall description of what your
## functions do
# These two functions can be used to create a list object which caches the 
# inverse of a matrix to save on computation.  makeCacheMatrix establishes the
# list based on a square matrix input, and cacheSolve is used to solve for the
# inverse on the first call and fetch the inverse on any proceeding calls. 

## Write a short comment describing this function
# This function creates a list which contains the matrix x. The inverse is 
# initially set to NULL, this state changes once cacheSolve is run, as it 
# changes the value of i to hold the inverted matrix. 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
# This takes the list holding the data generated in makeCacheMatrix.  If it
# has been run before it will return the already cached inverse of the matrix.
# If it has not been run, it will recognize that the inverse is still NULL and
# populate i with the inverse of the data. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
