## Cache the inverse of a matrix to avoid repeated matrix inversion caculations

## makeCacheMatrix takes in a matrix and return a list of functions:
## 1. set - store a new matrix
## 2. get - return the current matrix
## 3. setinverse - set the inverse of the current matrix
## 4. getinverse - return the inverse of the current matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes in a makeCacheMatrix object and return the inverse of
## the current matrix stored in that object. If an inverse is already cached,
## cacheSolve skips caculation and returns the cached value directly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cache data")
                return(i)
        }
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setinverse(inverse)
        inverse
}
