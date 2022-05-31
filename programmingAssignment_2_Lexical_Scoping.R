# Function makeCacheMatrix
# numeric() was substituted by matrix()
# The following changes were done:
#     setmean => setinv
#     function(mean) => function(inverse)
#     mean => inverse
#     getmean => getinv

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# Function cacheSolve
# The following changes were done:
#     x$getmean() => x$getinv()
#     mean(data, ...) => solve(data, ...)
#     x$setmean => x$setinv

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting inversed matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

# Matrix used to test both functions

m <- matrix(rnorm(9), 3, 3)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)
