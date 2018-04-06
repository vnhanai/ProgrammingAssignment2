makeMatrix <- function(x = matrix()) {
  n <- NULL
  
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  
  get <- function () x
  setinverse <- function(inverse) n <<- inverse
  getinverse <- function () n
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


cache_Inverse_Of_Matrix <- function (x,...) {
  n <- x$getinverse()
  
  if(!is.null(n)) {
    message("getting catched data")
    return(n)
  }
  
  data <- x$get()
  n <- solve(data,...)
  x$setinverse(n)
  n
}