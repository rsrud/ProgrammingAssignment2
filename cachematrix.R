## makeCacheMatrix - the function creates a special matrix-object that
## is able to cache its inverse


makeCacheMatrix <- function(x = matrix()){
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## cacheSolve - the function is used to compute the inverse of a special
## matrix returned by the function 'makeCacheMatrix'. If the inverse has
## already been calculated, then the function 'cacheSolve' should extract the
## inverse from the cache.

cacheSolve <- function(x, ...){
	m <- x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
## returning a matrix that is the inverse of 'x'
}
