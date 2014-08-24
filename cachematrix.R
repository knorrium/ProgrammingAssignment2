# the functions below allow us to calculate the inverse of a matrix,
# storing it in cache for faster calculations, assuming that the matrix
# provided is always inversible

makeCacheMatrix <- function(x = matrix()) {
  # this is where we store the cached calculation
  cachedMatrix <- NULL

  set <- function(y) {
    x <<- y
    cachedMatrix <<- NULL
  }

  # returns the original matrix
  get <- function() {
    return(x)
  }

  # stores the inverse matrix provided by the function
  setSolve <- function(m) {
    cachedMatrix <<- m
  }

  # returns the cached inverse matrix
  getSolve <- function() {
    return(cachedMatrix)
  }

  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

# given a cachedMatrix created with makeCacheMatrix(matrix), this function will
# calculate its inverse matrix
#
# example:
#
# > mat1 <- makeCacheMatrix(matrix(1:4, ncol=2, nrow=2))
#
# > mat1$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
#
# First call (no cached results):
#
# > cacheSolve(mat1)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# Second call (cached results):
#
# > cacheSolve(mat1)
# [1] "getting cached data"
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

cacheSolve <- function(x, ...) {
  m <- x$getSolve()

  # check whether we got a cached calculation and return it if so
  if (!is.null(m)) {
    print("getting cached data")
    return(m)
  } else {

    # if we didn't have it in the cache, calculate it and store it
    data <- x$get()
    m <- solve(data)
    x$setSolve(m)
    return(m)
  }
}
