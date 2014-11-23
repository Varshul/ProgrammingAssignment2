makeCacheMatrix <- function(x=matrix()) {
## Function to cache the inverse of a matrix using inverse function
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<-solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
 
}

cacheSolve <- function(x, ...) {
## Computes the inverse of the matrix returned
## by makeCacheMatrix(), unless the inverse has
## already been calculated, in which case
## it retrieves it from the cache.
    m <- x$getInverse()
    if ( ! is.null(m)) {
        print("getting cached data")
        return(m)
    }
    m <- solve(x$get())
    x$setInverse(m)
    m
}
 
 
 
y <- makeCacheMatrix(matrix(1:4,2))
y$get()
y$getInverse()
y$set(matrix(5:8,2))
y$get()
cacheSolve(y)
cacheSolve(y)
## shows the cached result of the inverse matrix
y$getInverse()
