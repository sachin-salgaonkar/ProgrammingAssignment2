## This function will store the matrix inputted
## and allow for the cacheSolve function to 
## calculate the inverse of said matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
  
}


## This is the function that actually computes the inverse.
## The reason that it checks the cache first is to save
## computing time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)){
            message("getting cached data")
            return (m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
  
}
