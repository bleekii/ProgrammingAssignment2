## cachematrix.R
## Functions: makeCacheMatrix
##            cacheSolve
##


## makeCacheMatrix
##  input:  any square matrix
##  output: a list including fuctions for acesses to the matrix and it's 
##          inverse.
##
##  The initial version of the the list doesn't include the inverse. Use  
##  cacheSolve() to solve the inverse.
##
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve
##  input:  a list created by makeCacheMatrix()
##  output: returns the inverse of maxtrix supplied in the input. 
##          
##  The inverse is only calculated once otherwise a cached version is returned.
##
cacheSolve <- function(x, ...) {       
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}