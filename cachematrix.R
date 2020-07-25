## Two functions 'makeCacheMatrix' and 'cacheSolve' are used to catch inverse of
## matrix of computing it repeatedly

## This function stores the value of inverse as cache and returns it when asked

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
          x<<-y
          m<<-NULL
    }
    get<-function() x
    setInverse<-function(inverse) m<<-inverse
    getInverse<-function() m
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function returns the inverse of matrix by computing or from the cache(if available)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getInverse
    print(m)
    if(!is.null(m)){
          message("Getting catched data")
          return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setInverse(m)
    m
}
