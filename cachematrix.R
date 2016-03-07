# Assignment: Caching the Inverse of a Matrix

# 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# 2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data_matrix <- x$get()
  m <- solve(data_matrix, ...)
  x$setinv(m)
  m
}

# Create a Matrix with Random numbers and applying the above Matrix inverse functions

set.seed(1110201)
r = rnorm(1000000)
mat = matrix(r, nrow=1000, ncol=1000)

temp = makeCacheMatrix(mat)

cacheSolve(temp)
