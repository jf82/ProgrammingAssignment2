## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## "makeCacheMatrix" function constructs a list of functions that set/get values
## for a matrix x, and set/get values for inverse of x.
## The calculation of inverse matrix is done by another function "cacheSolve".
## get: get values of matrix x.
## set: set values of matrix x.
## getInv: get vaues of inverse matrix.
## setInv: set values of inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(mInv) m <<- mInv
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function
## "cacheSolve" function calculates the inverse of matrix stored in "x" and caches the result for
## future calling of the function. If values of the matrix had been changed, this function
## recalculates the inverse matrix. 
## The function uses "solve" function. The arguments are matrix stored in "x", number of rows of 
## the matrix and other arguments passed to cacheSolve function.
## It is assumed that matrix stored in "x" is square and invertible, so errors and warnings 
## are not controlled.
## The function returns the inverse of x, but also sets values for future use.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    ##for solve function it is used diag(n) = identity matrix of n x n 
    n <- dim(data)[1]
    m <- solve(data, diag(n), ...) 
    x$setInv(m)
    m	
}
