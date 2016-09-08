#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not$

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix<-NULL
  #set the matrix
  set<-function(y){
    x<<-y
    inverseMatrix<<-NULL
  }
  #get the matrix
  get<-function() x
  #set the inverse of the matrix
  setInverse<-function(inv) inverseMatrix<<-inv
  #get the inverse of the matrix
  getInverse<-function() inverseMatrix
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function gets the mean of the "matrix" created by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  inverseMatrix<-x$getInverse()
  #If inverse already calculated, the results will return cached value
  if(!is.null(inverseMatrix)){
    message("getting cached matrix inverse")
    return(inverseMatrix)
  }
  #If it is a new one, calculate matrix inverse and cache it in x
  data<-x$get()
  inverseMatrix<-solve(data)
  #cache calculated matrix inverse
  x$setInverse(inverseMatrix)
  inverseMatrix
}


