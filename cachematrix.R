## Functions below calculate the inverse of a given matrix and stores it in cache. However, if the inverse was calculated previously
## then the inverse already stored in cache is retrived and returned

## This function creates a list of methods to operate on matrix passed as argument (an empty matrix by default)
makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function takes a matrix as an argument and calculates the inverse. If inverse was cached previously, it just returns that one
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting inverse matrix from cached data")
                return(inv)
        }
        m <- x$get()
        inv <- solve(m, ...)
        x$setinv(inv)
        inv
}
