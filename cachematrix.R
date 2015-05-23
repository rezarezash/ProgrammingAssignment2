## Create a cahcable matric inverse
makeCacheMatrix <- function(mat = matrix()) {
  l <- NULL
  setValue <- function( matrix ) {
    mat <<- matrix
    l <<- NULL
  }
  getValue <- function() {
    mat
  }
  setInverse <- function(inverse) {
    l <<- inverse
  }
  getInverse <- function() {
    l
  }
  
  list(set = setValue, get = getValue,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculate the inverse of the special matrix 
cacheSolve <- function(x, ...) {
  i <- x$getInverse()  
  if( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  data <- x$getValue()
  i <- solve(data) %*% data  
  x$setInverse(i)
  
  i
}
