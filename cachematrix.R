## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
  m<-NULL
  
  ## Method to set the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  ## Method the get the matrix
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  
  ## Return a list of the methods
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
  
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Compute the inverse of the special matrix returned by "makeCacheMatrix"
  ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  
  ## Just return the inverse if its already set
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  matrix<-x$get()
  
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
