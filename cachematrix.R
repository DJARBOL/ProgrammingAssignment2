## R Programming - Assingment 2
## Daniel Arboleda M.

## According to the assignment guidelines, this functions are
## meant to calculate the inverse of a square invertible matrix
## and store it in the cache so that R identifies if it has been
## calculated before and takes it from the memory.

## This function creates a matrix that cache's its inverse

makeCacheMatrix <- function(x = matrix()) {
  MatrixInv <- NULL
  SetMat <- function(y) {
    x <<- y
    MatrixInv <<- NULL
  }
  GetMat <- function() x
  SetMatInv <- function(solve) MatrixInv <<- solve
  GetMatInv <- function() MatrixInv
  list(SetMat = SetMat, GetMat = GetMat,
       SetMatInv = SetMatInv,
       GetMatInv = GetMatInv)
}


## This function calculates the inverse of the matrix with the
## above function, unless it is already calculated, in which case,
## it takes the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
  MatrixInv <- x$GetMatInv()
  if(!is.null(MatrixInv)) {
    message("getting cached inverted matrix")
    return(MatrixInv)
  }
  MatData <- x$GetMat()
  MatrixInv <- solve(MatData, ...)
  x$SetMatInv(MatrixInv)
  MatrixInv
}

##Time to test the functions
MT1 <- matrix(1:4, 2, 2)
t1 <- makeCacheMatrix(MT1)
t1$GetMat()
t1$GetMatInv()

cacheSolve(t1)

MT2 <- matrix(c(5, 8, 4, 7, 3, 6, 6, 2, 1), 3, 3)
t2 <- makeCacheMatrix(MT2)
t2$GetMat()
t2$GetMatInv()
cacheSolve(t2)
