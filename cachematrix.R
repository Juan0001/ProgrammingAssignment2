## cachematrix.R consists a pair of functions (makeCacheMatrix and
## makeCacheMatrix )that cache the inverse of a matrix.


## makeCacheMatrix:
## is a function that creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get the value of the matrix
    get <- function() x
    
    ## coompute the inverse of the matrix
    setInverseMatrix <- function(solve) m <<- solve
    
    ## get the inversed matrix
    getInverseMatrix <- function() m
    
    list(set = set, get = get,
    setInverseMatrix = setInverseMatrix,
    getInverseMatrix = getInverseMatrix)
}


## cacheSolve:
## is a function that computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverseMatrix()
    
    if(!is.null(m)) {
        message("Getting cached data!")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setInverseMatrix(m)
    m
}