## Put comments here that give an overall description of what your
## functions do

## Some kind of OOP in R, where 4 methods are stored as list items, setsolve actually calculating a matrix inverse and keeping it in memory

makeCacheMatrix <- function(x = numeric()) {
     m <- NULL
     set <- function(y) {
         x <<- y
         m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
 }
 
 cacheSolve <- function(x, ...) {  ## checks if x has been processed as an MakeCacheMatrix object and returns the inverse starting with call #2
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
