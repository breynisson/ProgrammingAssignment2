

makeCacheMatrix <- function(x = matrix()) {
  # Makes a matrix and caches its inverse.
  # Args:
  #    x: is a matrix, which we assume is invertable.
  #
  # get() returns the matrix passed to makeCacheMatrix.
  # set() passes sets new values for the matrix.
  # set_inverted() calculates the inverted matrix of x.
  # get_inverted() returns the inverted matrix of x.
  inverted_matrix<-NULL
  set<-function(y){
    x<<-y
    inverted_matrix<<-NULL
  }
  get <- function() x
  set_inverted <- function(solve) inverted_matrix <<- solve
  get_inverted <- function() inverted_matrix
  list(set = set, get = get,
       set_inverted = set_inverted,
       get_inverted = get_inverted)

}

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  # Will first check if the inverse has already been calculated.
  inverted_matrix <- x$get_inverted()
  if(!is.null(inverted_matrix)){
    message("getting cached data")
    return(inverted_matrix)
  }
  data <- x$get()
  inverted_matrix <- solve(data, ...)
  x$set_inverted(inverted_matrix)
  inverted_matrix
}

