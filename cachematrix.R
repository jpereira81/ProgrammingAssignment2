## The functions below returns the matrix inverse of a defined matrix using stored  
## information in the cache to save time and capacity of computation

## makeCacheMatrix creates a matrix for the purpose of solving the matrix inverse.
## The output of this firts function is a list containig set, that sets the matrix,
## get, that gets this matrix that is solved in setsolve and then stored in getsolve.
## To call the makeCacheMatrix define it an object, like x (e.g. x <- makeCacheMatrix(matrix())). 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cachesolve tests wether the matrix inverse is already stored in chache, if not it generates
## the matrix inverse of the matrix defined within the function makeCacheMatrix. To call 
## cachesolve, just use the object defined above (e.g. cachesolve(x))

cachesolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
