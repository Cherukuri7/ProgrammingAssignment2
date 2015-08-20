## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x=matrix()) {
        m <-NULL
        set <- function (y){ 
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