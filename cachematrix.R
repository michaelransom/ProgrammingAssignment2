## makeCacheMatrix: sets matrix, gets matrix, sets inverse, gets inverse

makeCacheMatrix <- function(x = matrix()) {
      inv = NULL
      set = function(y) {
            x <<- y
            inv <<- NULL
      }
      get = function() x
      setinverse = function(inverse) inv <<- inverse 
      getinverse = function() inv
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve: checks if inverse is there, if not it calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv = x$getinverse()
      if (!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      mat.data = x$get()
      inv = solve(mat.data, ...)
      x$setinverse(inv)
      return(inv)
}
