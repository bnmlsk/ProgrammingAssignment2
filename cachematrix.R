## Put comments here that give an overall description of what your
## functions do

#' This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() { x }
  setinverse <- function(inverse) { m <<- inverse }
  getinverse <- function() { m }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#' This function computes the inverse of the special "matrix" 
#' returned by \code{\link{makeCacheMatrix}} function. If the 
#' inverse has already been calculated (and the matrix has not
#' changed), function retrieves the inverse from the cache.
#' 
#' @param x a cache wrapper on a matrix created using \code{\link{makeCacheMatrix}}
#' @usage
#' x <- makeCacheMatrix(matrix(c(1, 4, 0, 2, 5, 8, 3, 6, 9), nrow = 3))
#' cacheSolve(x)
#'  
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}