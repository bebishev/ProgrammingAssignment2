## Usage of this code:
## > source("cachematrix.R")
## > m = matrix(c(1,1,0,1), nrow=2, ncol=2)
## > x = makeCacheMatrix(m)
## > cacheSolve(x)
##      [,1] [,2]
## [1,]    1    0
## [2,]   -1    1
## > cacheSolve(x)
## getting cached data
##      [,1] [,2]
## [1,]    1    0
## [2,]   -1    1

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    r <- NULL
    set <- function(y) {
        x <<- y
        r <<- NULL
    }
    get <- function() x
    setres <- function(res) r <<- res
    getres <- function() r
    list(set = set, get = get,
         setres = setres,
         getres = getres)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    res <- x$getres()
    if(!is.null(res)) {
        message("getting cached data")
        return(res)
    }
    data <- x$get()
    res <- solve(data)
    x$setres(res)
    res
}
