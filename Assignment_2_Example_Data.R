makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

## test data

a <- makeVector(c(1,2,3,4))
a$get()
a$getmean()
cachemean(a)
a$set(c(10,20,30,40))
a$getmean()
cachemean(a)
cachemean(a)
a$get()
a$setmean(0)  # do NOT call setmean() directly despite it being accessible for the reason you will see next
a$getmean()
a$get()
cachemean(a)
a <- makeVector(c(5, 25, 125, 625))
a$get()
cachemean(a)
cachemean(a)