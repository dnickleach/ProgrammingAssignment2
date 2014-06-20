## This pair of functions can be used to cache the inverse of a square 
##(invertible) matrix

##This function creates a special "matrix" object that can cache its inverse.
##The special matrix object is a list containing 4 functions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #Create function to set the value of the matrix in cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #Create function to get the matrix 
  get <- function() x
  
  #Create function to set the value of the inverse in cache
  setinv <- function(inverse) inv <<- inverse
  
  #Create function to get the value of the inverse
  getinv <- function() inv
  
  #Object to be returned will be a list object of all 4 functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


##This function computes the inverse of the special "matrix" returned
##by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve
##the inverse from the cache.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  #Get inverse from cache
  minv <- x$getinv()
  
  #If matrix inverse already exists then retrun it and exit function
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  
  #Inverse does not already exist - get matrix
  data <- x$get()
  #Calculate inverse
  minv <- solve(data, ...)
  #Set matrix inverse in cache
  x$setinv(minv)
  return(minv)
}
