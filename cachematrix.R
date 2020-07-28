##This function will create a special matrix that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
  p<-NULL
  setmatrix <- function(y){
    x <<- y
    p <<- NULL
  }
  getmatrix <- function()x
  setInverse <- function(inverse) p <<- inverse
  getInverse <- function() p 
  list(setmatrix = setmatrix, getmatrix = getmatrix, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function
##This function compute the inverse if it is cached by the previous function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  p <- x$getInverse()
  if(!is.null(p)){
    message("getting cached data")
    return(p)
  }
  data <- x$getmatrix()
  p <- solve(data)
  x$setInverse(p)
  p 
}
