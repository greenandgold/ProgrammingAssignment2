## makeCacheMatrix takes a matrix and returns a list of four functions.
## The first function "set" sets the value of the matrix. 
## ("set" is only needed if we want to change the matrix.)
## The second function "get" gets the value of the matrix.
## The third function "setinverse" sets the value of the inverse of the matrix.
## The fourth function "getinverse" gets the value of the inverse of the matrix.

## cacheSolve returns the inverse of the matrix given to makeCacheMatrix.
## cacheSolve first checks to see whether the inverse is cached.
## If the inverse is not cached, cacheSolve computes the inverse and caches it.

################################################################################

## makeCacheMatrix returns a list of functions in preparation for cacheSolve

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		 setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of x

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached inverse")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i       
}
