## Author: AHMED SHAKIB
## Inverse Matrix caching
## Inverse caching is useful to do the matrix calculation easier by not doing repetedly 
## matrix storing and inverse it
## function calling 
## One function for make cache matrix
## another one is for cachesolve


## first function creates a special matrix 

makeCacheMatrix <- function(x = matrix()) {  ## matrix generates 
      inv <- NULL   ## NULL declared 
      set <- function(y) {    ## Set value for the function
              x << - y      ## compare two function
              inv <<- NULL
      }

      get <- function () x
      
      setInverse <- function(inverse) inv<<- inverse ## inverse function set up
      getInverse <- function() inv   ## Retrieve inverse function
      list (set = set,
            get = get,
            setInverse = setInverse,
            getInverse = getInverse)


}


## Below function inverse the matrix 

## above matrix is needed if the inverse has not computed yet

## Retrieve the inverse from the cache 

##  mat function 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse() ## inverse function generate 
        if (!is.null(inv)) {
                message("We will get cached data what we expected")  ## message creates when cached data 
                return(inv)  ## return the value of INV
        }

        mat <- x$get()
        inv <- solve(mat, ...)   ## solve funciton after cached
        x$setInverse(inv)
        inv
}
