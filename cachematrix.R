## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  #initialize object to store "special" matrix
  xinverse <- NULL
  set <- function(y) {
    x <<- y #assign input argument in parent environment
    xinverse <<- NULL #clears any value of x1 in cache 
  }
  get <- function() x #retrieve x from parent enviro
  #assign "inverse" to the value of xinverse in the parent environment 
  setinverse <- function(inverse) xinverse <<- inverse  
  #uses lexical scoping to find and return the xinverse
  getinverse <- function() xinverse 
  #name everything so can use x$ to call by name in the parent enviro
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the matrix from makeCacheMatrix.
## If the inverse is already calculated, then it will retreive the inverse from the Cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #Retrieve the cached value stored in makeCacheMatrix() evironment, if there is one
  m <- x$getinverse()
  #check if the value is null 
  if (!is.null(m)) {
    #if its not null (i.e. has been defined), return the value
    message("getting cached data")
    return (m)
  }
  #if m is null (!null = FALSE) - calculate and set the inverse
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
