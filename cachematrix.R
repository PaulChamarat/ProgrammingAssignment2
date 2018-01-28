## Put comments here that give an overall description of what your
## functions do

## The function will create special matrix to record the matrix inverse.



## Write a short comment describing this function
## makeCacheMatrix creates a special matrix
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  get<-function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
}


## Write a short comment describing this function
## The cacheSolve function will calculate hte inverse of the matrix created by the
## makeCacheMatrix. First, it will check whether inverse has already been calculated.
## If the inverse has not been previously calculate, it will calculate and cache the 
## inverse value with setInverse function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
