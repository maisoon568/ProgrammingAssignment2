##  Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## In this assignment we write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL   ## first setting m as a null matrix
     
     set <- function(y) {    ## setting the value of the matrix
         x <<- y
         m <<- NULL
     }
     
     get <- function() x     ## getting the value of the matrix
     
     setinverse <- function(inverse) m <<- inverse   ## setting the value of the inverse
     
     getinverse <- function() m     ## getting the value of the inverse
     
     list(set = set, get = get,      ## listing the set, get, setinverse, getinverse functions
          setinverse = setinverse,
          getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  
     ## checking if the inverse has been calculated
     m <- x$getinverse()
  
     ## getting the inverse from the cache if the inverse has already been calculated
     if(!is.null(m)) {    
         message("getting cached data")   
         return(m)
      }
  
     ## calculating the inverse of the data and setting the value of the inverse in the cache via the setinverse function if the inverse has not been calculated
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
        ## Return a matrix that is the inverse of 'x'
}
