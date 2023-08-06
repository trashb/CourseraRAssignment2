makeCacheMatrix <- function(x = matrix()) {
#   caches the inverse of a matrix
#   input: matrix
#   output: list (containing the inverse of the matrix)
    
    inv <- NULL
    # set matrix value:
    set <- function(y) {                                    
        x <<- y
        inv <<- NULL
    }
    # get matrix value
    get <- function() {x}
    # set inverse value
    setsolve <- function(inverse) {inv <<- inverse}  
    # get inverse value
    getsolve <- function() {inv}                      
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


cacheSolve <- function(x, ...) {
#   tries to get cached inverse of matrix; calculates it if cache DNE
#   input: list (call to makeCacheMatrix)
#   output: matrix (the inverse of the matrix)
    
    inv <- x$getsolve()
    # get cache if inverse has been calculated
    if(!is.null(inv)){                
        message("Getting cached data")
        return(inv)
    }
    # compute inverse if inverse has not been calculated
    originalmatrix <- x$get()       
    inv <- solve(originalmatrix, ...)
    x$setsolve(inv)
    inv
}
