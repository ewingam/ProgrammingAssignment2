## The two functions below calculate and cache the inverse of a matrix so that 
## if called again when the matrix has not changed the code grabs the cached 
## result instead of re-calculating the inverse.

## The first function, makeCacheMatrix creates a list containing a function that
## does the following: 
## 1. set the initial value of the matrix. 
## 2. get the value of the matrix.
## 3. set the value of the inverse of the matrix.
## 4. get the value of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}

## This function takes the above list as an input. 
## It checks to see if the inverse has already been calcuted, and if so, returns
## the cached result. If not, it performs the calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
