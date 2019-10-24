## These functions firstly create a function that gives the inverse of a matrix
## and then calculates the inverse of the matrix. If the inverse has already
## been calculated, the result is retrieved from the cached data.

## makeCacheMatrix creates a set of functions - get, set, gst.inverse and
## get.inverse and returns them to the global environment in the form of
## a list

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set.inverse <- function(solve) m <<- solve
        get.inverse <- function() m
        list(set = set,
             get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse)

}


## cacheSolve gives the inverse of the matrix that is fed in as an argument
## by using the functions defined in makeCacheMatrix and executing the solve()
## function.
## It checks to see if the data has already been calculated - if it has it
## will return the result that has already been cached. 

cacheSolve <- function(x, ...) {
        m <- x$get.inverse()
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set.inverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}


