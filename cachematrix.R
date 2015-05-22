## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix which is able to create its own inverse. The k object returned by the function is a list.
makeCacheMatrix<- function(x=matrix()){
        m <- NULL
        set<- function(y) {
                x<<- y
                m<<- NULL
        }
        get<- function() x
        setinverse<-function(solve) m<<-solve
        getinverse<-function() m
        k<-list(set=set, get=get,
                setinverse=setinverse,
                getinverse=getinverse)
        return(k)
        
}


## Write a short comment describing this function-- This function take in the matrix and checks
##if the inverse  ofthe same has already been saved in the cache or not. 
##Thus, the recomputation of the inverse ofthe matrix is avoided if its already there in the cache.

cacheSolve<- function (x, ...) {
        m<-x$getinverse()
        if (!is.null(m)){
                message ("getting cached data")
                return (m)
        }
        data<-x$get()
        m<-solve(data, ...)
        x$setinverse(m)
        return(m)
        ## Return a matrix that is the inverse of 'x'
}
