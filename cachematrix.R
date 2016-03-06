
## This function creates a "special" vector to save/retriev results of the the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  o <- x
  s <- NULL

  setsolved <- function(y) {
    s <<- y
  }
  
  setorig <- function(y) {
    o <<- y
  }
  
  getsolved <- function() s
  getorig <- function() o
  
  list(getsolved = getsolved, getorig = getorig, setsolved = setsolved, setorig = setorig)
}


## This function calculate or return stored inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(is.null(x$getsolved())) {
    s <- solve(x$getorig())
    x$setorig(x)
    x$setsolve(s)
    s
  }
  else {
    message("getting solved matrix")
    x$getsolved()
  }
  

}
