
## the make cache function to save the matrix in environment with the associated setter/getter matrix and 
## setter/getter mean
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
  
  ##set mean
  setmean <- function(mean) m <<- mean
  
  ##get mean
  getmean <- function() m

  ##return a list of the upper function
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


##calculate the mean of the special list created in the above function
cachemean <- function(x, ...) {
  
  ##if the value is already in cache return 
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##otherwise calculate the mean and set in cache
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  
  ##return
  m
}
