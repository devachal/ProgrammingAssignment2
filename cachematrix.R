## makeCacheMatrix creates a matrix and stores the inverse of it.
## cacheSolve function calculates the inverse of the matrix in case it's not already calculated and saved. 
##If it's already calculated then it will take it from cache/



makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <-function(y) {
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                getinv <- function() i
                setinv <- function(inv) i <<- inv
                list(get=get,set=set,getinv=getinv, setinv=setinv)
}



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
