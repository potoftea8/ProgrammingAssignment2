## Cache the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        ## Set the value of the matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        ## Get the value of the matrix
        get <- function() x
        ## Set the value of the inverse of the matrix
        setsolve <- function(solve) s <<- solve
        ## Get the value of the inverse of the matrix
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...) 
        x$setsolve(s)
        s
}
