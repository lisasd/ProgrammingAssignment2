## This function creates a special "matrix"
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      ## set the special "matrix"
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      ## get the special "matrix"
      get <- function() x
      ## inverse the special "matrix"
      setInverse <- function(solve) inv <<- solve
      ## return the inverse of the special "matrix"
      getInverse <- function() inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function computes the inverse of the special
## "matrix" returned by "makeCacheMatrix" above

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if(!is.null(inv)) {
            message("getting cache matrix")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInverse(inv)
      inv
}
