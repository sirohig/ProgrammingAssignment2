## Matrix inversion is a costly computation and these functions allow caching of inverse of a matrix
## instead of computing it repeatedly
## Functions use function 'solve()' to inverse the matrix.

## This function creates a special matrix object that can cache its inverse
## The vector that it creates a list containging following functions to work on the input matrix 'x'
## set() set the value of the matrix
## get() get the value of the input matrix
## setinv() set the value of the inverse of the input matrix
## getinv() get the value of the invers matrix
## based on makeVector function in assignment text

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <-function(y) {
                x<<- y
                m<<-NULL
        }
        get <-function() x
        setinv <-function(solve) m<<- solve
        getinv <- function() m
        list(set=set,get=get,
             setinv=setinv,
             getinv=getinv)
}

#This function computes the inverse of the specail matrix returned by makeCacheMatrix function
#based on cacheMean function in assignment text
#Checks if the inverse matrix has been already computed, returns cached value else computes and saves it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <-x$getinv()
        if(!is.null(m) ){
                message("getting cached data")
                return(m)
        }
        data <-x$get()
        message("computing again")
        m<-solve(data)
        x$setinv(m)
        m
}
