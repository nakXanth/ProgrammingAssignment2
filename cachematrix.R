
## Write a short comment describing this function
## Makes the caching, and creates 4 methods in the vector

makeCacheMatrix <- function(x = matrix()) {


        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        ##getting and setting is the same, need to change the methods when doing the calc
        ##using variable i as the 'inverse' matrix value
        setinv <- function(solve) i <<- solve ##assume this can be used, as instructed
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)



}


## Write a short comment describing this function
##runs the solve or it uses the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i

}