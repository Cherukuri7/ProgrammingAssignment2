## The following two functions allow caching the inverse of a Matrix

## First function create a list of functions to validate
## the input matrix for squreness and then cache its inverse

makeCacheMatrix <- function(x=matrix()) {
        m <-NULL
        set <- function (y){ 
                ## check for squreness
                check <- class(try(solve(y),silent=T))=="matrix"
                if(check==TRUE){
                        x <<- y      
                        m <<- NULL}
                else{
                        stop("Matrix must be squre")}
                
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## Second function cachesolve() computes the inverse
## of the matrix and returns either from cache or
## recalculates.

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInv(m)
        m
}