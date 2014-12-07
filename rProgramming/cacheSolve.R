cacheSolve <- function(x, ...) {
  m <- x$getMatrix()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m
}