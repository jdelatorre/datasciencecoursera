## These functions will create a matrix and cache it's inverse for future use

## This function creates a matrix and sets the environment for which
## variables will be stored.

makeCacheMatrix <- function(x = matrix()) {
        inverse1 <- NULL
        set  <- function (y) {
                x <<- y
                inverse1 <<- NULL
        }
        
        get <- function () x
        setinverse <- function (solve) inverse1 <<- solve
        getinverse <- function () inverse1
        list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## This function will check to see if the inverse is stored, if not  
##it will calculate the inverse matrix and cache the result.

cacheSolve <- function(x, ...) {
        
        inverse1 <- x$getinverse()
        if(!is.null(inverse1)) {
                message("getting cached data")
                return(inverse1)
        }
        data <- x$get()
        ## Return a matrix that is the inverse of 'x'
        inverse1 <- solve(data, ...)
        x$setinverse(inverse1)
        inverse1
}

