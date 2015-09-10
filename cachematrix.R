## makeCacheMatrix will return a special list that is utilized by cacheSolve.
## cacheSolve uses the information from makeCacheMatrix to determine if it 
## needs to solve for the inverse, or if it can pull it from the cache.

## makeCacheMatrix will return a special list containing a function to:
## Get the value of the matrix
## Set the value of the matrix
## Set the value of the inverted matrix
## Get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
 	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverted <- function(solve) m <<- solve
        getInverted <- function() m
        list(set = set, get = get,
             setInverted = setInverted,
             getInverted = getInverted)
}


## This function will return an inverted matrix by solving the matrix, or
## pulling it from the cache if applicable.

cacheSolve <- function(x, ...) {
	  m <- x$getInverted()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverted(m)
        m

        ## Return a matrix that is the inverse of 'x'
}
