
##function cache matrix stores a list of matrix functions 
makeCacheMatrix <- function(x = matrix()) {
  ##initialize m
  m <- NULL 
  ##sets the matrix stored in the main function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x ## gets the matrix x stored in the function
  
##setinv and getinv are like  set and get. They don't calculate the inv, 
##they simply store the value of the input in a variable m into the main function makecacheMatrix (setinv)
  ##and return it (getinv).
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  
  ##the following list stores a list of matrix functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##check if the matrix inverse is cached if not run the inverse function 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##subset the list of functions to get the matrix inverse
  m <- x$getinv()
  ## the if statement is there to check if the matrix inverse is already cached 
  ##if it is return the matrix inverse and come out of the function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##it will come here if it is not cached and calculate the matrix inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
