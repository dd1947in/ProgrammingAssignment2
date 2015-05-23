## Coursera : Data Science : R-Programming  : Programming Assignment 2 : Peer Evaluation
## Assignment for demonstrating lexical scoping in R

## makeCacheMatrix  makes a matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    ix <- NULL
    set <- function(y) {
    x <<- y
    ix <<- NULL
  }
  get <- function() x
  setinv <- function(inv) ix <<- inv
  getinv <- function() ix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns cached inverse if it exists, else computes inverse and returns

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ix <- x$getinv()
  if(!is.null(ix)) {
    message("getting cached inverse")
    return(ix)
  }
  data <- x$get()
  ix <- solve(data, ...)
  x$setinv(ix)
  ix  
} 
