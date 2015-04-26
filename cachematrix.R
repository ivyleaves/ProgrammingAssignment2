## Put comments here that give an overall description of what your
## functions do

## The below function is used to compute the inverse of the matrix if not already computed and 
# stores in cache in order to save computation cost so that it can be pulled if it hasn't changed 
# whenever needed in order to save computation cost.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  InverseofMatrix <- NULL
  set <- function(y) {
    x <<- y
    InverseofMatrix <<- NULL
  }
  get <- function() x
  setInverseofMatrix <- function(inverse) InverseofMatrix <<- inverse
  getInverseofMatrix <- function() InverseofMatrix
  list(set = set, get = get,
       setInverseofMatrix = setInverseofMatrix,
       getInverseofMatrix = getInverseofMatrix)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  InverseofMatrix <- x$getInverseofMatrix()
  if(!is.null(InverseofMatrix)) {
    message("getting cached data")
    return(InverseofMatrix)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverseofMatrix(inverse)
  inverse
  
  
  
}
