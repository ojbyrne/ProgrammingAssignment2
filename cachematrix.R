## This inverts matrices with caching.

## Create a caching matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## invert the matrix with caching

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	## check that the matrix is square

	if (dim(x$get())[1] != dim(x$get())[2]) stop('matrix is not square')
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
