cacheSolve <- function(x, ...) {
  ##computes the inverse of the special "matrix" returned by makeCacheMatrix above
  ##if the inverse has already been calculated (and the matrix has not changed), 
  ##then the cachesolve should retrieve the inverse from the cache
  m <- x$getInv() #gets the value of the inverse matrix
  if(!is.null(m)) {
    ##if m already exists (the inverse is already calculated)
    message('getting cached data')
    return(m)
  }
  
  #if m doesn't exist, calculate the inverse of the matrix
  data <- x$get() #gets the value of the matrix
  m <- solve(data, ...) #calculates the inverse
  x$setInv(m) 
  ##now set this newly calculated inverse matrix in the parent env
  ##chache it so the next time you won't have to recalculate
  m #return the inverse matrix
}
