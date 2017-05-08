## Used together, these functions can calculate the
## inverse of a matrix while considering possibility
## of a cached matrix to increase efficiency by avoiding
## unnecessary calculation.

## This first function creates a special matrix object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## Next, define the four functions to be
        ## used as input for cacheSolve
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        ## Next, return these functions in a list.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Second function takes input of first function
## to compute inverse of the matrix. If it already
## has been computed, it will simply use cache instead.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        ## If inv is already computed, return its value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## If inv has not been computed, calculate and 
        ## return the result.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
