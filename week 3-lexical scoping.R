#  Caching the inverse of a matrix
 
makeCacheMatrix() <- function(x = matrix()) {
 inv <- NULL
 set <- function(y) {
   x <<- y
   inv <<- NULL
 }
 get <- function() x
 setinverse <- function(inverse) inv <<- inverse()
 getinverse <- function() inv
 list(set = set,get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}

#Solving Cache
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cache matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv  <- solve(data,...)
  x$setinverse(inv)
  inv
}

