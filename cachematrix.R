
## the make cache function to save the matrix in environment with the associated setter/getter matrix and 
## setter/getter inverse
makeCacheMatrix <- function(x = numeric()) {
  ##matrix field
  m <- NULL
  
  ##set matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##get matrix
  get <- function() x
  
  ##set inverse
  setinverse <- function(inverse) m <<- inverse
  
  ##get mean
  getinverse <- function() m

  ##return a list of the upper function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##calculate the mean of the special list created in the above function
cacheinverse <- function(x, ...) {
  
  ##if the value is already in cache return 
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##otherwise calculate the mean and set in cache
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
  ##return
  m
}
