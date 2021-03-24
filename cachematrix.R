## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix, really a list containing functions that 
# set and get the value of a matrix, and also set and get the value of it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL        #empty matrix 
  set <- function(y) {   #defines set function
    x<<-y
    inverse<<- NULL
  }
  get<-function() {
      x
  }
  setInverse <- function(inv) {
    inverse<<- inv
  }
  getInverse<- function() {
    inverse
  }  
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix
#above. If the inverse has already been calculated (and the matrix has not changed),
#then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse() 
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse<- solve(data,...)
  x$setInverse(inverse)
  inverse
}
