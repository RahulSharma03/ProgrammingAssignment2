## Objective is to create functions to calculate inverse if SQUARE (only) matrix and cache it for further usage.
## i.e. inverse calculation for same matrix will not be computed after first time. For every subsequent 
## calls results will be fetch from cache memory.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

   m <- NULL # initializing m
        set <- function(y) {
							x <<- y #
							m <<- NULL
							}
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv) # Creating list of function.
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		 m <- x$getinv() # getting cached inverse matrix output
        if(!is.null(m)) {
						message("getting cached Matrix Inverse")
						return(m) # returning cached inverse matrix.
						}
        data <- x$get() # getting square matrix
        m <- solve(data, ...) # Creating inverse matrix m using solve function.
        x$setinv(m) # caching inverse matrix output
        m # returning calculated inverse matrix for first time.
}
