# programming assignment 2 of Coursera course rprog-004
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# First of a pair of functions that cache the inverse of a matrix. See also cacheSolve

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	## set is not strictly necessary here, as it is never used. Its function is to provide something similar to an OOP setter method
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	## "getter", return value from closure (that entered the closure via the argument to the function makeCacheMatrix)
	get <- function() x
	## set the cached value for the inverse
	setinverse <- function(inverse) s <<- inverse
	## return the cached value for the inverse
	getinverse <- function() s
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.
# Second of a pair of functions that cache the inverse of a matrix. See also makeCacheMatrix

cacheSolve <- function(x, ...) {
	## has the mean inverse previously computed? If so, use the computed value and return to caller
	s <- x$getinverse()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	## if not, get the data and compute the inverse
	data <- x$get()
	s <-  solve(data, ...)
	
	## cache the computed value
	x$setinverse(s)
	## Return a matrix that is the inverse of 'x'
	s
}
