# makeCacheMatrix: Purpose is to store a martix and a cached value of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL

  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  get<-function() x
  setmatrix<-function(solve) m<<- solve #store the matrix
  getmatrix<-function() m               #retrieve the matrix
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
# function calculates the inverse of a special matrix created with makeCacheMatrix
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix() # if a cached value exists return it
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get() #otherwise store the value
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
