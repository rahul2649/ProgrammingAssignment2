
## I have created two functions here, 1st function makeCacheMatrix will take as input 
## the matrix for which we want the inverse. The output of this function will act as the input for the 
## cacheSolve function

## This functions takes a matrix as input and returns a special vector which returns set, get, setinverse and getinverse
## This function is will assign Null value to 'm' everytime this functions is called
## However if the function is not called and an already existent output of this function is used by cacheSolve
## then it returns the inverse value from the cache.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes the output of the makeCacheMatrix function as a input
## and checks if the Inverse is already existing, if yes then it returns the cached value 
## other wise it computes the value and then sets the cache with the computed value 
## which will be used the next time if the same value if required
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}        ## Return a matrix that is the inverse of 'x'
}
