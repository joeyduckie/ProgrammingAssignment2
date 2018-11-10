## Programming Assignment 2
## Coursera - Johns Hopkins R Programming


## Function will create a special Matrixthat can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    set_xInverse <- NULL
    
    #set matrix which forces the Inverse to Null
        setmatrix <- function (x){
            set_x <<- x
            set_xInverse <<- NULL
        } 
    #get Matrix
        getmatrix <- function() set_x
    
    #set matrix inverse
        set_mInverse <- function(y) set_xInverse <<- y
    #get matrix inverse
        get_mInverse <- function() set_xInverse
        
    #return a list
        list(set = setmatrix, get = getmatrix, seti = set_mInverse, geti = get_mInverse)
}


## Write a short comment describing this function

## Function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$get()
    #print(m)
    i <- x$geti()
    
    if(!is.null(i)){
        message("...getting from cache")
        return(i)
    }else if(det(m)==0){
        message("Cannot Compute an inverse. Determinant is 0!")
        return(NULL)
    }else{
        i <- solve(m, ...)
        x$seti(i)
        x$geti() 
    }
    
}
