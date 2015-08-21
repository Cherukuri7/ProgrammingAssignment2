## The following two functions allow caching the inverse of a Matrix

## First function create a list of functions to validate
## the input matrix for squreness and then cache its inverse

makeCacheMatrix <- function(x=matrix()) {
        m <-NULL
        set <- function (y){ 
                ## check for squreness of input matrix
                check <- class(try(solve(y),silent=T))=="matrix"
                if(check==TRUE){
                        x <<- y      
                        m <<- NULL}
                ## stop setting the matrix if not squre
                else{
                        stop("Matrix must be squre")}
                
        }
        get <- function() x    ## provides input to cacheSolve function
        setInv <- function(s) m <<- s   ## Stores inverse calculated by cacheSolve
        getInv <- function() m   ## provides comparison to cacheSolve
        
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## Second function cachesolve() computes the inverse
## of the matrix and returns either from cache or
## recalculates.

cacheSolve <- function(x, ...) {
        m <- x$getInv()  ## returns NULL when executed first time
        if(!is.null(m)) {
                message("getting cached Inverse Matrix")
                return(m)
        }
        else {  ## This code executes if m is NULL
                matrx <- x$get()  ## retrieves matrix
                m <- solve(matrx)  ## solves for inverse
                x$setInv(m)  ## sets m value in main function using special operator
        }
        m   ## returns inverse matrix
}