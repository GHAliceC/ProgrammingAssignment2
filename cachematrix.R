## this function aims to cache the inverse of a matrix, 
##it contains the following steps:
## i. set the value of the matrix
## ii. get the value of the matrix
## iii. set the values of the inverse
## iv. get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # invs stores the inversed matrix result
  invs <- NULL
  # here set the value of the matrix
  set <- function(y){
    x <<- y
    invs <<- NULL
  }
  # here get the value of the inversed matrix
  get <- function() x
  set_invs <- function(inversed) invs <- inversed
  get_invs <- function() invs
  list(set = set_invs, get =  get_invs)
}


## this function computes the inversed matrix retruned by makeCacheMatrix.
## If the inverse has already been calculated or the matrix has not been
## changed, this function will retrieve the inversed matrix from the 
## cache and skip the computation.

cacheSolve <- function(x, ...) {
  # check if the matrix is inversed, if so, return the inverse from cache
  invs <- x$get_invs
  if(!is.null(invs)){
    message("getting cached data")
    return(invs)
  }
  # calculate and cache the inverse of the matrix and return 
  matrix <- x$get()
  invs <- solve(matrix,...)
  x$set_invs(invs)
  invs
}
