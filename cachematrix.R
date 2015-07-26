## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.

## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  ## creates a special matric object that can cache its inverse
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


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

## Sample execution
## > y <- matrix(1:4,2,2)
## > y
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > z <- makeCacheMatrix()
## > z$set(y)
## > cacheSolve(z)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
