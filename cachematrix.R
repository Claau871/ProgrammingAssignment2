# This R script creates two functions: makeCacheMatrix & CacheSolve to be used for submission
# for Assignment 2 of R-programming, John Hopkins University, Coursera.org
# 
# Function 1: makeCacheMatrix
# 
# This function creates a "special" vector, which is really a list containing 4 functions:
# set, get, setinverse and getinverse
#
# The first element can be used to 'set' the cached matrix, 'get' will return the cached matrix,
# 'setinverse' can set the matrix's inverse in cache, and 'getinverse' will return the cached inverse

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

# Function 2: cacheSolve
# 
# This function returns a matrix that is the inverse of x. However, x is a special object 
# and may have cached attributes. 
#
# cacheSolve will check to see if x's inverse has already been calculated. If it has, it will
# return this. If not, it will calculate the inverse of x and return this, and at the same time 
# store this calculated inverse in cache for future use.

cacheSolve <- function(x, ...) {
  # return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached inverse data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  message("caching inverse data")
  m
}

# END
