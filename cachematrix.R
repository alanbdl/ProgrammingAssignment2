## Developing two functions to creat a matrix and them caching 
## the inverse of this matrix


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
}
	get <- function() x
	SetInverse <- function(inverse)m <<- inverse
	GetInverse <- function() m
	list(set = set, get = get, SetInverse = SetInverse, 
	GetInverse = GetInverse)

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$GetInverse()
	if(!is.null(m)) {
		message("getting cached data")
            return(m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$SetInverse(m)
	m
}
