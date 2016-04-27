## Summary of the Project
## The computation of matrix inverse is a computation intensive operation. 
## We will store the result of matrix inverse (after first invocation); thus,
## the cached value will be returned if the matrix is not modified.
## 
##
## makeCacheMatrix() is used to create an object containing matrix 
## and get and set functions to retrieve and update the matrix respectively.
## It stores the result of inverse matrix. 
## Further, if the matrix is modified (using set function), the cached value is
## removed (initialized to NULL).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The cacheSolve() calls the solve() function; however, it caches the value 
## for future calls.
## For the first invocation of the cacheSolve() function, it will compute the 
## inverse of the matrix and stores the result. For all future requests, the 
## cached value is used, provided the cached value is not null.
##
## Square Matrix Condition: It checks that matrix must be square; otherwise, 
## error message is printed and the original matrix is returned.
##
## Singular Matrix Condition: It checks that determinant is not zero; otherwise,
## error message is printed and the original matrix is returned.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  if (nrow(data) != ncol(data)){
    message("Not a square matrix")
    message(paste("rows =", nrow(data), "cols =", ncol(data)))
    message("returning the original matrix")
    return (data)
  }
  else if (det(data) == 0){
    message("singular matrix: determinant is zero")
    message("returning the original matrix")
    return (data)
  }
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# Testing Case 1: non-square matrix
m <- makeCacheMatrix(matrix(1:80, 10,8))
m$get()
cacheSolve(m)

# Testing Case 2: singular matrix
m <- makeCacheMatrix(matrix(1:100, 10,10))
m$get()
cacheSolve(m)

# Testing Case 3: non-singular square matrix
m <- makeCacheMatrix(matrix(c(1,4,7,2,2,8,3,6,9), 3,3))
m$get()
cacheSolve(m)

# Testing Case 4: Using cache value
cacheSolve(m)

# Testing Case 5: Modify the matrix
m$set(matrix(c(1,4,8,2,2,8,3,6,9), 3,3))
m$get()
cacheSolve(m)