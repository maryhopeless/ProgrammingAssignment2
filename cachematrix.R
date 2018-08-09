## Caching the Inverse of a Matrix

require(MASS) 

## Cache vector calculating

makeCacheMatrix <- function(x = matrix()) {
        
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setinvrs <- function(ginv) invrs <<- ginv
        getinvrs <- function() invrs
        list(set = set, get = get,
             setinvrs = setinvrs,
             getinvrs = getinvrs)
}

## Calculating of inverse matrix

cacheSolve <- function(x, ...) {
        invrs <- x$getinvrs()
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        data <- x$get()
        invrs <- ginv(data, ...)
        x$setinvrs(invrs)
        zapsmall(invrs)
        
}
