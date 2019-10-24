## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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

a <- matrix(1:4, 2, 2)
b <- makeCacheMatrix(a)
cacheSolve(b)
