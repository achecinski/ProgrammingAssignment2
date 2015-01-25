## Calculating the inverse of a matrix can be time consumming.
## It's better to do it once and store the result in a cache.
## This is the purpose of the following functions.
## They provide all the functionnalities needed to calculate the inverse of a matrix and cache it.

## Allows to create an object that contains a matrix and which can store it's inverse after it has been calculated
## This way it's inverse is cached and will be calculated only once.
## If the matrix is reset using the set function, the cache is emptied.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Calculates the inverse of a matrix stored in a "CacheMatrix" object
## If the "CacheMatrix" object does not contain the inversed matrix in it's cache
## it will be calculated and then stored  in the "CacheMatrix" object's cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
