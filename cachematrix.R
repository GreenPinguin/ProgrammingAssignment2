## These functions makes it possible to chache the result of a 
## matrix inversion. Once the result is cached, future calls to 
## the inversion function will give instantaneous results. 
##
## This feature is decomposed in two distinct function : 
## - "makeCacheMatrix" which return a special matrix object able 
##   to store a cached value for its inverse. 
## - "cacheSolve" which take the special matrix object returned
##   by the makeCacheMatrix() function and return the invert of 
##   the matrix (as a standard matrix object).


## The makeCacheMatrix function takes a matrix and restur a
## special matrix object able to store a cached value for its
## inverse. 
##
## Arguments: 
## x: a matrix (default to an empty matrix)
##
## Return value: 
## A special matrix object able to store a cached value for its
## inverse. The returned object is in fact a list with 4 functions
## allowing to interact with the matrix object : 
## - get() which return the underlying matrix object
## - set(y) which can assign y to the underlying matrix object
##   This function will erase the cached value of the inverse matrix
## - getinverse() which return the cached value of the inverse
##   If no value was cached, NULL is returned
## - setinverse(inv) which cached inv (the inverse of the matrix) 
##   for later use
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    getinverse <- function() i
    setinverse <- function(inv) i <<- inv
    list(get = get, set = set, 
         getinverse = getinverse, setinverse = setinverse)
}


## The cacheSolve function take a special matrix object built from
## the makeCacheMatrix function and returns the inverse of the 
## underlying matrix object. If the arguments matrix has a cached
## inverse matrix, it is returned immediatly. Else, the invert is 
## computed and the result is cached within the argument. 
##
## Arguments: 
## x: a special matrix object built from the makeCacheMatrix function
## ...: additionnal arguments to use with the standard solve function. 
## 
## Return value: 
## A standard matrix object which is the inverse of the matrix in 
## argument. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
