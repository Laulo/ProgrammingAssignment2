## Two functions that cache the inverse of a matrix.

## `makeCacheMatrix`: creates a special "matrix" object that can cache its
## inverse.
## Warning: the code assumes that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	# sets the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

	# gets the value of the matrix
        get <- function() x

	# sets the value of the inverse
	setInverse <- function(inverse) inv <<- inverse

	# gets the value of the inverse
        getInverse <- function() inv

	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## `cacheSolve`: computes the inverse of a special "matrix" like the one
## returned by `makeCacheMatrix` above. If the inverse has already been
## calculated (and the matrix has not changed), then `cacheSolve` retrieves
## the inverse from the cache.

cacheSolve <- function(x) {
	inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
