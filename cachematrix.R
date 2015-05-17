## Functions that set and cache the inverse of a matrix

## creates a matrix and sets up the storage of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  ##initialize the cache matrix
  m <- NULL
  
  ##set the matrix to the parameter value
  set <- function(y) {
    x <<- y
    ##clear the cached value
    m <<- NULL
  }
  
  ##get the matrix value back
  get <- function() x
  
  ##set the inverse of the matrix and cache
  setinverse <- function(solve) m <<- solve
  
  ##get the inverse of the matrix from the cache
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## solves the inverse of the matrix or retrieves the value from cache if it's previously been calculated
cacheSolve <- function(x=matrix(), ...) {
  ##get the cached matrix
  m <- x$getinverse()
  m
  ##if cached matrix is not null then return value from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##if no cache, then get the mean, cache it and return the value
  data <- x$get()
  ##calculates the inverse and store
  m <- solve(data, ...) 
  x$setinverse(m)
  m
}

##check work
c <- matrix(c(4,2,7,6),2,2)
c1 <- makeCacheMatrix(c)
cacheSolve(c1)
cacheSolve(c1)
c %*% c1$getinverse()