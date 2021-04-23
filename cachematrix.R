## A pair of functions that cache the inverse of a matrix

## makeCacheMatrix function creates a matrix that can cache its 
## inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve function computes inverse of matrix returned by
## makeCacheMatrix
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## Testing on a simple 2x2 matrix
testMatrix <- matrix(c(1,2,3,4),2,2)
testMatrix
cacheMatrix <- makeCacheMatrix(testMatrix)
cacheSolve(cacheMatrix)