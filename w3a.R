makecacheMatrix <- function(x=matrix()){
  #x is defined as a matrix
  im<- NULL    # inversed matrix will be hold in im(inversed matrix) variable
  set<- function(y){
    x<<- y
    im<<- NULL
  }
  get<- function()x
  setinv<- function(inv) im<<- inv
  getinv<- function() im     #get the inversed matrix
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

# below the function to calculate the inv mat is defined:
cacheSolve<- function(x,...){
  im<- x$getinv
  if(!is.null(im)){
    message("cached data")
    im
  }
  mtd<- x$get()         # matrix is gotten(not inversed mat)
  im<- solve(mtd,...)   # inverse mat is obtained using solve function
  x$setinv(im)
  im
}
