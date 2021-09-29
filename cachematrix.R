## This is my submission on lexical scoping for week 3 of R Programming on Coursera
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() {x}
      setinverse <- function(inverse) {inv <<- inverse}
      getinverse <- function() {inv}
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...){
      inv <- x$getinverse()
      if(!is.null(inv)){
            message("getting the cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setinverse(inv)
      inv
}
