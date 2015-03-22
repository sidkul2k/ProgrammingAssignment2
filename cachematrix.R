#creates a vector for x matrix & its inverse

makeCacheMatrix <- function(x<-matrix())
{
  xin<-NULL #inverse
  set<-function() #sets the matrix in global env
  {
    x<<-y
    xin<<-NULL
  }
  get<-function() x #returns the matrix
  setinverse <- function(inverse) xin<<-inverse #sets inverse
  getinverse <- function(inverse) xin #gets inverse
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#function solves matrix, takes vector as input
cacheSolve <- function(x,...)
{
  xin<-x$getinverse()
  if(!is.null(xin)){
    message("getting cached data")
    return(xin)
  }
  mtx<-x$get()
  xin<-solve(mtx)
  x$setinverse(xin)
  xin #returns inverse of x
}