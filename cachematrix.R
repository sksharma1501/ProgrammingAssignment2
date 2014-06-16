# R Programming - Coursera
# Programming Assignment 2

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# It has four subfunctions: 
# set --> set a new matrix value
# get --> get the cached matrix value
# setinv --> set the inverse matrix value
# getinv --> get the cached inverse matrix

makeCacheMatrix <- function(x = matrix())
{
  invMat <- NULL
  set <- function(y)
  {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setinv <- function(inv)
  {
    invMat <<- inv
  }
  getinv <- function() invMat
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
# retrieves the inverse from the cache.
# Otherwise it computes the inverse and caches it.
cacheSolve <- function(x, ...)
{
  inv <- x$getinv()
  if (!is.null(inv))
  {
    message("gettting cached value")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}

# Test functions
mat1 <- makeCacheMatrix() 
mat2 <- mat1$get()
mat2
mat1$set(matrix(c(1,2,3,4,5,6,7,7,9), nrow=3, byrow=TRUE))
mat2 <- mat1$get()
mat2
invmat2 <- cacheSolve(mat1)
invmat2
invmat2 <- cacheSolve(mat1)
invmat2

mat1$set(matrix(c(1,2,2,4,5,6,7,8,8), nrow=3, byrow=TRUE))
mat2 <- mat1$get()
mat2
invmat2 <- cacheSolve(mat1)
invmat2
invmat2 <- cacheSolve(mat1)
invmat2
