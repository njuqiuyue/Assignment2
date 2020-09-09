#install and load the package matlib, which allows the usage of function inv
install.packages("matlib")
library(matlib)

makeCacheMatrix <- function(x = matrix()) {
  result <- NULL
  set <- function(y) {
    x <<- as.matrix(y)
    result <<- NULL
  }
  get <- function() x
  setInversion <- function(Inversion) result <<- Inversion
  getInversion <- function() result
  list(set = set, get = get,
       setInversion = setInversion,
       getInversion = getInversion)
}
#
#
cacheSolve <- function(x, ...) {
  result <- x$getInversion()
  if(!is.null(result)) {
    message("getting Inversion data")
    return(result)
  }
  data <- x$get()
  print(data)
  result <- inv(data, ...)
  x$setInversion(result)
  result
}

#below are my test codes
myMatrix<-makeCacheMatrix(matrix(c(1,5,11,15), nrow=2, ncol=2))
myMatrix$get()
myMatrix$getInversion()
cacheSolve(myMatrix)
