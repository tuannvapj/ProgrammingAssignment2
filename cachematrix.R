## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function that input a matrix and stores cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y){
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) iv <<- inverse
  getinverse <- function() iv
  list(
    set=set, get=get, 
    setinverse=setinverse, getinverse=getinverse
  )
}


## Write a short comment describing this function
## The function returns the inverse of a matrix if it has been cached
cacheSolve <- function(x, ...) {
  iv <- x$getinverse()
  if (!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  m <- x$get()
  m_solve <- solve(m)
  x$setinverse(m_solve)
  m_solve
}

