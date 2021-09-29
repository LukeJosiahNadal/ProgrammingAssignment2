## This is my submission on lexical scoping for week 3 of R Programming on Coursera

## The fundamental function here is makeCacheMatrix

## This function has set, get, setInv and getInv
## All of these functions are made use of in the following code
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL       ##This will initialize the inverse as NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() {x}         ##Then the function gets the matrix x
      setinverse <- function(inverse) {inv <<- inverse}
      getinverse <- function() {inv}
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...){
      inv <- x$getinverse()
      if(!is.null(inv)){ ##Checking if the inverse process would be NULL
            message("getting the cached data")
            return(inv) ##Then returns if so
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setinverse(inv)
      inv ##Finally, a matrix returned by which x is of inverse
}
