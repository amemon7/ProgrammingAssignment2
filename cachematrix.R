require(MASS)
makeCacheMatrix <- function(x = matrix()) { 
  minv <- NULL                     
  set <- function(y) {                      
    x <<- y
    minv <<- NULL              
  }
  get <- function() x
  #using ginv function from MASS - Solve can also be used instead
  setinv <- function(ginv) minv <<- ginv 
  getinv <- function() minv        
  list(set = set, get = get,                    
       setinv = setinv,
       getinv = getinv)
}

cacheSolve<- function(x, ...) {                 
  minv <- x$getinv()
  if(!is.null(minv)) {                 
    message("amemon7:Getting Cached Data")
    return(minv)
  }
  data <- x$get()                               
  minv <- ginv(data, ...)
  x$setinv(minv)
  minv
}