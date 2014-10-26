## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly. Below are two functions that are used to create a special object that stores a numeric matrix and caches its inverse matrix. 

## The contents of this file is based on the template taken from https://github.com/rdpeng/ProgrammingAssignment2

## This function creates a special "matrix" object that can cache its inverse. This object is really a list containing a series of functions to get and set the value of the matrix, to get and set the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        
        set <- function(y) {
                x <<- y # This assignment do not create new variable, but instead searches a variable in the parent environtment and assigns value to it
                im <<- NULL
        }
        
        get <- function() x
        setinv <- function (inv) im <<- inv
        getinv <- function () im
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function inverts the special matrix created with the above function. However, it first checks to see if the inverted matrix has already been calculated, if so, if gets the inverted matrix form the cache and skips the computation. Otherwise, it calculates the inverted matrix and stores the result on the chache via setinv function. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinv()
        if(!is.null(im)){
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinv(im)
        im
}