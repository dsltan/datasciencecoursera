###PROGRAM ASSIGNMENT 2

#Reference used: https://github.com/DanieleP/PA2-clarifying_instructions

rm=(list=ls())

## The function makeCacheMatrix() stores 4 other functions as objects in a list:
#   get() returns the existing input matrix x
#   set(y) updates the input matrix x with a new matrix y (if needed)
#   setmatrix() stores the solve() solution (inverse matrix) into the variable invx
#   getmatrix() returns the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    set <- function(y){
        x <<-y
        invx <<-NULL
    }
    get <- function() x
    setmatrix <- function(solve) invx <<- solve  
    getmatrix <- function() invx
    list(set=set, get=get, setmat=setmatrix, getmat=getmatrix)
}


##The cacheSolve function computes the inverse of matrix created in the 
#  makeCacheMatrix function.
#  cacheSolve takes makeCacheMtrix(x) as an arugment.
#  It checks if the matrix inverse has been calculated - 
#    If yes, retrieve the inverse value from the cache and return that value
#    If no, use the matrix data from x$get() to compute the inverse matrix, store
#    the computed inverse matrix in the cache with x$setmat(), and return the
#    the inverse matrix value.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invx <- x$getmat()
    if(!is.null(invx)) {
        message('use cached data')
        return(invx)
    }
    datamat <- x$get()
    invx <- solve(datamat, ...)
    x$setmat(invx)
    invx
}

## EXAMPLE:  calling the functions to compute matrix inverse
#x <-matrix(c(1,-0.25, -0.25, 1), nrow=2)
#x
#a <- makeCacheMatrix(x)
#a$set(x)
#cacheSolve(a)
## Assign new matrix for inverse computation
#a$set(matrix(1:4,2,2))
#a$set(x)
#a$get()
#cacheSolve(a)
