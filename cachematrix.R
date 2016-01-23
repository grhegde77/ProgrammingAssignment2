## Following functions demonstrate the usage of operator <<-
## To demonstarte matrix inverse has been used

## This function saves the matrix inverse in the cache
## if the matrix and inverse is not changed then it returns data from cache

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        ##Caches the matrix and resets the matrix inverse
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        ##returns original matrix
        get <- function() x
        ##caches the matrix inverse
        setinv <- function(inv) n <<- inv
        ##return the matrix inverse
        getinv <- function() n
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function tries to get the matrix inverse from cache if not available then
## it calcluates and saves in the cache
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ##Tries to get matrix inverse from the cache
        n <- x$getinv()
        if(!is.null(n)) {
                ##got the matrix inverse from cache
                message("getting cached data")
                return(n)
        }
        ##no cache available then it calculates matrix inverse
        data <- x$get()
        ##Calculates the inverse of matrix
        n <- solve(data, ...)
        ##Saves the matrix inverse in the cache
        x$setinv(n)
        n
}
