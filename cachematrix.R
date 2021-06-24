## makeCacheMatrix and cacheSolve are used to create an object-like list
## for a square invertible matrix and then solve for the inverse of that matrix.
## The inverse is cached for later use to speed up program execution.

## The makeCacheMatrix function takes a matrix as input and returns a list of 
## functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix (and cache the value)
## 4. get the value of the inverse of the matrix
## Note: The matrix is assumed to be a square invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## The cacheSolve function calculates the inverse of a "makeCacheMatrix" matrix
## as created above. If this value has already been cached, cacheSolve simply
## returns the value without having to do any further resource draining calculations.

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of x
  m <- x$getmatrix()
  if(!is.null(m)) {
    # m has a cached value, retrieve that value
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) # solve the inverse matrix of x if it is not already cached
  x$setmatrix(m)
  m
}

## Below is some example code that can be used to test the above functions.
M <- matrix(c(7, 3, -2, 5), nrow=2, ncol = 2)
expected <- solve(M)

cachetest <- makeCacheMatrix(M)
cachesolvetest <- cacheSolve(cachetest)

cachesolvetest == expected
## Should return a matrix of TRUE values such as:
# 
#      [,1] [,2]
# [1,] TRUE TRUE
# [2,] TRUE TRUE

## To test if the cached value is being properly stored and retrieved, try:
cachesolvetest2 <- cacheSolve(cachetest)
## The message "getting cached data" will be printed to the console 
## if the function is operating properly, such as:
# 
# > cachesolvetest2 <- cacheSolve(cachetest)
# getting cached data

## Can test makeCacheMatrix() and cacheSolve() functions with other invertible matrices
