# Assignment: Caching the Inverse of a Matrix
#
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly (there
# are also alternatives to matrix inversion that we will not discuss here).
#
# Computing the inverse of a square matrix can be done with the solve function
# in R. For example, if X is a square invertible matrix, then solve(X) returns
# its inverse.

# makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverseValue <- NULL
        setMatrix <- function(y) {
                if (!is.matrix(y)) {
                        stop("The parameter x must be matrix !")

                } else if (ncol(y) != nrow(y)) {
                        stop("The parameter x must be a square invertible matrix !")

                }
                x <<- y
                inverseValue <<- NULL

        }
        getMatrix <- function() x
        setInverse <- function(inverse) inverseValue <<- inverse
        getInverse <- function() inverseValue
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should etrieve the
# inverse from the cache.
cacheSolve <- function(x, ...) {
        inverseValue <- x$getInverse()
        if(!is.null(inverseValue)) {
                message("getting cached data")
                return(inverseValue)
        }
        data <- x$getMatrix()
        inverseValue <- solve(data, ...)
        x$setInverse(inverseValue)
        inverseValue
}
