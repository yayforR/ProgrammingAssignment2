## Functions below catches the inverse of a matrix. 

## To create the cache matrix, I created a function of x = matrix. Then I set this to makeCacheMatrix. 
## Within the function, I used solve to compute the inverse of the square matrix. 

makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## The function cacheSolve below returns the inverse of the matrix created by the above function. 


cacheSolve <- function(x, ...)
{
  m <- x$getsolve()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
