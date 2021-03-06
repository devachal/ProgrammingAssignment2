makeCacheMatrix <- function(x = matrix()){
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
