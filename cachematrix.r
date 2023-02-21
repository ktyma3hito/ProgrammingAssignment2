makeCacheMatrix <- function( m = numeric() ) {
    i <- NULL

    # set
    set <- function( value ) {
        m <<- value
        i <<- NULL
    }

    # get
    get <- function() {
        m
    }

    # set inverse
    setInverse <- function(solve) {
        i <<- solve
    }

    # get inverse
    getInverse <- function() {
        i
    }

    # list function
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# check the function
# bb <- makeCacheMatrix(matrix(data=c(1,2,3,4),nrow=2,ncol=2))
# bb
# bb$get()

cacheSolve <- function(x, ...) {
    m <- x$getInverse()

    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
	
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
# check the function
# cacheSolve(bb)
