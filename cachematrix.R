## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## Below function helps understanding scoping rules and stores matrices in memory

makeCacheMatrix <- function(x = matrix()) {
  spinv <- NULL
  set <- function(y) {
    x <<- y
    spinv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) spinv <<- inverse
  getinverse <- function() spinv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
##Below function provides inverse if already available in cache. 
##If not, computes and sets it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  spinv <- x$getinverse()
  if(!is.null(spinv)) {
    message("getting cached data.")
    return(spinv)
  }
  data <- x$get()
  spinv <- solve(data)
  x$setinverse(spinv)
  spinv
}