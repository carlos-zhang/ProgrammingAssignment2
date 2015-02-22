##Those functions chache  the result of matrix's iverse caculation.Ana
## read the result from the cache next time if the iverse has already been 
## caculated.

## This function creates a special "matrix" object 
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- invserse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInvserse,
             getInverse = getInvserse)

}


## This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated 
##(and the matrix has not changed), #
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInvers()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}
