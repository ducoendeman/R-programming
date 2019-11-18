## makeCacheMatrix creates an R object that stores a matrix and its inverse.
## It builds a set of functions and returns the functions within a list to the
## parent environment.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## initialize inv as NULL; will hold value of matrix inverse 
        set <- function(y) { ## define the set function to assign new
                x <<- y      ## value of matrix in parent environment
                inv <<- NULL ## reset inv to NULL
        }
        get <- function() x ## returns value of the matrix
        setinverse <- function(inverse) inv <<- inverse ## assigns value of inv in parent environment
        getinverse <- function() inv ## gets the value of inv where called
        ## In order to refer to the functions with the $ operator list of functions is returned
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cascheSolve requires an argument that is returned by makeVector() in order to
## retrieve the mean from the cached value that is stored in the makeVector()
## object's environment.
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        }

