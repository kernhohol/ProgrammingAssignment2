## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## I check, if the cacheSolve function hd been ever run
## If it run once, the las_cached matrix variable is set and can be compared to the one in makeCacheMatrix input
## Also, in this case, I use previously used "m" value instead of settin it to NULL

makeCacheMatrix <- function(x = matrix()) {
  
  if(exists("last_cached") && dim(x)==dim(last_cached) && isTRUE(all.equal(x,last_cached))){
    message("same matrix")
    m <- m2
    data <- NULL
    set <- function(data) {
      x <<- data
    m <<- m2
    }
    
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
  }
    
  
  else {
    
  m <- NULL
  data <- NULL
  set <- function(data) {
    x <<- data
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }
}


## Write a short comment describing this function
##

cacheSolve <- function(x, ...) {
  
  m  <- x$getinverse()
  
    if(!is.null(m)) {
  
    message("getting cached data")
    return(m)
  } else 
    message("calculating data")

  data <- x$get()
  x$set(data)
  m <- solve(data,...)
  x$setinverse(m)
  last_cached <<- x$get()
  m2 <<- m
  m
  ## Return a matrix that is the inverse of 'x'
}

