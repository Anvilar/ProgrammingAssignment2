
## This functions are part of the programming assignment 2 from Data Science course. 
## The following functions are used to  create a special object that stores a matrix and caches its inverse. 
## For this assignment, assume that the matrix supplied is always invertible.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y){
                x<<- y
                m<<- NULL
            }
            get<-function() x
            setsolve<- function(solve) m <<- solve
            getsolve<- function() m
            list(set=set,get=get,
                 setsolve=setsolve,
                 getsolve=getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
       m <-x$getsolve()
       if(!is.null(m)){
           message("getting cached data")
           return(m)
       }
       data<-x$get()
       m<-solve(data, ...)
       x$setsolve(m)
       m
}
