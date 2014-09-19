
## Here are a pair of functions that catch the inverse of a matrix.

## func1: to create a special "matrix" object that can catches its inverse
makeCacheMatrix <- function(x = matrix()) {   
    ix <- NULL
    set<-function(y){
        x<<-y
        ix<<-NULL
    }
    get<-function()x
    setinverse <-function(inverse) ix<<-inverse ## inverse matrix
    getinverse <- function() ix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)## output a special "matrix"    
}

## func2: to compute the inverse of the special "matrix" returned by the makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## check whether input is a square matrix
    m <- NULL    
    m <- x$getinverse()
    mx <- x$get()
    is.matrix(mx)
    if (!is.null(m)){ ## if inverse matrix exists, skip the calculation
        message("getting cached data")
        return(m)        
    }
    data<-x$get()
    m<-solve(mx,...) ## calculate inverse matrix
    x$setinverse(m)
    m
}