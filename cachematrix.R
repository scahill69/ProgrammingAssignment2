## This function creates an R object that stores a matrix and its inverse, to
## be used in the cacheSolve function below. It builds a set of functions
## and returns these functions within a list to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y){
      x <<- y
      m <<- NULL
  }
  get <- function() x
  
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  list(set = set,            # gives the name 'set' to the set() function defined above
       get = get,            # gives the name 'get' to the get() function defined above
       setsolve = setsolve,  # gives the name 'setsolve' to the setsolve() function defined above
       getsolve = getsolve)  # gives the name 'getsolve' to the getsolve() function defined above
}


## This function calculates the inverse of the special matrix created with the
## makeCacheMatrix function above. It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates
## the inverse of the matrix and sets the value of the inverse in the cache via the setsolve function.
## Note that this function assumes square matrix x is invertible!

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Check if inverse already calculated/stored in m; if so, return that.
  m <- x$getsolve()
  
  if(!is.null(m)){
      message("getting cached data")
      return(m)
  }
  
  ## If inverse not already calcuated/stored in m, then calculate it.
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}
