## Create a cache enabled matric object that is able to cache the inverse of the matrix
## parameter x is the original matrix object
makeCacheMatrix <- function(x = matrix()) {
  
  ## m is used to store the inverse of the matrix
  m <- NULL
  
  ## sets the alue of the original matrix and reinitalize m to NULL  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## gets the value of the original matrix
  get <- function() {
    x
  }
  
  ## sets the inverse of the original matrix and assigns it to m
  setInverse <- function(matrix) {
    m <<- matrix
  }
  
  ## gets the inverse of the original matrix
  getInverse <- function() {
    m
  }
  
  ## lists the operation that can be done on the object
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## computes the cache of the matrix for the first time and then caches it
## for subsequent calls
## a paramter of makeCacheMatrix is passed 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## If the inverse is not null, that means we already computed it and we can return the 
  cached inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## at this point, it means we didn't compute the cache yet, so get the original matrix
  data <- x$get()
  
  ## compute its inverse
  m <- solve(data, ...)
  
  ## now store its inverse in the cache
  x$setInverse(m)
  
  ## returns the inverse
  m
}
