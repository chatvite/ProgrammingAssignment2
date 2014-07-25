## The following function will make a cache matrix. 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function will return the inverse of a matrix that has been passed to
## makeCacheMatrix.  

cacheSolve <- function(x, ...) {
   i <- x$getinv()
   if (!is.null(i)) {
       message("getting cached data")
       return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinv(i)
   i
}
