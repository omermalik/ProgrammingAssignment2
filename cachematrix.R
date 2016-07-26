## Write a short comment describing this function

## The purpose of this function is to create a special "matrix" object that can cache its inverse.
## the function supports setting matrix, getting matrix, setting inverse and getting
## inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(invers) inv <<- invers
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Function returns a matrix that is the inverse of 'x' if it hasn't been cached
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("Getting Cached inv data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}
