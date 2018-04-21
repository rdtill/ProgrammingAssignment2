## Put comments here that give an overall description of what your
## functions do
#
#  makeCacheMatrix and cacheSolve allow for the storage of the inverse of a 
#  provided matrix. This allows the program to save time by avoiding calculating
#  the same inverse multiple times, and instead simply calling after it has been 
#  calculated once.



## Write a short comment describing this function
#
#  makeCacheMatrix initializes the provided matrix and i, as well as four
#  functions (set, get, setinv, getinv) in it's environment.  It then 
#  returns a list of the four initialized functions, so that they may be stored
#  and called at a later time.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Write a short comment describing this function
#
#  cacheSolve takes the output of makeCacheMatrix as its input and uses the 
#  getinv() to get the inverse of the provided function.If the inverse has 
#  been calculated, it will return the value of i, which was obtained through 
#  calling the getinv() function.If the inverse has not been calculated yet, 
#  it will calculate it using the solve() funcion.


cacheSolve <- function(x, ...) {
  i <- x$getinv()        ## Return a matrix that is the inverse of 'x'
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  invmat <- x$get()
  i <- solve(invmat, ...)
  x$setinv(i)
  i
}
