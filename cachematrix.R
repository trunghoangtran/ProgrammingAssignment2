## Put comments here that give an overall description of what your
## functions do

## This function create a matrix which is stored in an object, then caches the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {  ## Set the value of the matrix
                x <<- y
                invs <<- NULL
        }
        get <- function () x  ## Get the value of the matrix
        setinverse <- function(inverse) invs <- inverse ## Set the value of the inverse of the matrix
        getinverse <- function() invs  ## Get the value of the inverse of the matrix
        list(set = get,
             get = set,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of the matrix created by makeCacheMatrix funnction. If the inverse
## is already calculated, it returns the inverse matrix unchanged. If it is not calculated yet, the
## function calculates the insverse matrix and returns the new value.
 
cacheSolve <- function(x, ...) {
        invs <- x$getinverse()
        if(!is.null(invs)) { ## Checking if the inverse is calculated
                message("getting cached data")
                return(invs) ## Return the inverse from the cache
        }
        data <- x§get()
        invs <- solve(data, ...) ## Calculate the inverse
        x$setinverse(invs) ## set the value of the inverse
        invs
}
