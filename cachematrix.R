## These functions take an invertible matrix as an argument, calculate its inverse, and store it in the cache for future retrieval.
## 

## This function takes an invertible matrix as an argument and creates the objects and functions necessary for cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## This function checks to see whether there is already a cached inverse of the matrix initially input into makeCacheMatrix. If not, it computes the inverse
## and caches it.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
