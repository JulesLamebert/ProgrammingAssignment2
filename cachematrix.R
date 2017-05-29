## function that put the information about the matrix and the inverse.

# x is a matrix. We assume that x is inversible.

makeCacheMatrix <- function(x = matrix()) {
  
  # default value before we calculate it
  i <- NULL
  
  #if we want to set matrix a certain value after to have create the object
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # function that return the value of the matrix
  get <- function() x
  
  # funtion to save the value of the inverse
  setinverse <- function(inver) i <<- inver
  
  # function that return the value of the inverse
  getinverse<- function() i
  
  # retunr the list of function so that they are available
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## function that return the inverse of a matrix that have been encode as makeCacheMatrix
## if the matrix inverse have never been calculated, then it calculate it and store in makeCacheMatrix object
## if the the matrix inverse have already calculated, it's return the value 

# x is a makeCacheMatrix object

cacheSolve <- function(x, ...) {
       
  # take the inverse from the x object(that is a makeCacheMatrix object)
  i <- x$getinverse()
  
  # if we have already calculated the inverse, return the result
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # give the value of the matrix to data
  data <- x$get()
  # calculate the inverse of the matrix
  i <- solve(data, ...)
  # save the result of the inverse matrix
  x$setinverse(i)
  # return the value
  i
}
