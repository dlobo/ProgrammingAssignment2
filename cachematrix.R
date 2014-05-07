## This set of functions caches a matrix and its inverse. Since inverse
## is an expensive operation it does it only once and returns the cached
## inverse when needed

## You can use the functions as follows
## s <- makeCacheMatrix(m)
## t <- cacheSolve(s)
## t will have a cached copy of s

# Takes a matrix as an argument and caches the matrix locally
# exposes setter and getter for the matrix and the inverse of the matrix (solv)

makeCacheMatrix <- function(x = matrix()) {
    solve <- NULL
    cacheMatrix <- x
    set <- function(y) {
        cacheMatrix <<- y
        solve <<- NULL
    }
    get <- function() cacheMatrix
    setsolve <- function(inv) solve <<- inv
    getsolve <- function() solve
    list(
        set = set,
        get = get,
        setsolve = setsolve,
        getsolve = getsolve
        )
}

##

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if (!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, , ...)
    x$setsolve(s)
    s
}
