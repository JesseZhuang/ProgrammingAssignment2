## the first function makeCacheMatrix creates a list of 4 functions
## 1 set the value(s) of the matrix
## 2 get the value(s) of the matrix
## 3 set the value(s) of the inverse matrix
## 4 get the value(s) of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setinv <- function(inverse) inv <<- inverse
   getinv <- function() inv
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## the cacheSolve function calls x$get if no inv exist in cache
## this will return the value(s) set by makeCacheMatrix function
## cacheSolve will compute the inverse matrix and return it
## if inv exists in cache and not Null, will print message
## and return the cached inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv <- x$getinv()
   if(!is.null(inv)) {
      message("getting cached inverse matrix")
      return(inv)
   }
   mat <- x$get()
   inv <- solve(mat)
   x$setinv(inv)
   inv
}
