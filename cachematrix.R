## This .R file consists of two functions:  makeCacheMatrix for to make a special "matrix", whose inverse can be stored
## in the cache. The second function cacheSolve checks, if the inverse of a matrix made with makeCacheMatrix already exists
## in the cache and returns this, except it has not been cached or the matrix has changed, in which case it calculates the
## the inverse again

## makeCacheMatrix makes a special "matrix", whose inverse can be stored. A list of four functions is created, which when
## when called return the matrix (get), can be used to set the matrix to a new value (set), retrieve the inverse (getinverse)
## or allow to set the inverse (setinverse)

makeCacheMatrix <- function(x = matrix()) { ##input 
        i <- NULL                           ##set inverse to NULL (not existing)
        set <- function(y) {                ##function set, which allows to change the matrix
                x <<- y
                i <<- NULL                  ##resets previous inverse to NULL
        }
        get <- function() x                 ##function to return the matrix
        setinverse <- function(inverse) i <<- inverse ## function to set the inverse
        getinverse <- function() i                    ## function to return the inverse of the matrix
        list(set = set, get = get,                    ## list of the functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve checks, if the inverse of a matrix made with makeCacheMatrix already exists
## in the cache and returns this, except it has not been cached or the matrix has changed, in which case it calculates the
## the inverse again

cacheSolve <- function(x, ...) {        ##input
        i <- x$getinverse()             ##gets inverse of matrix in chache or NULL if non existent
        if(!is.null(i)) {               ##checks if inverse exists
                message("getting cached data") ## message that inverse will be taken from the cache
                return(i)               ##returns cached inverse
        }
        data <- x$get()                 ##gets original matrix, if no inverse is cache
        i <- solve(data, ...)           ##calculates inverse
        x$setinverse(i)                 ##stores inverse in cache
        i                               ##returns inverse
}
