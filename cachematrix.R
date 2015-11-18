## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.

## This script makes it possible to cache the inverse of a matrix
##
## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse.
##
## makeCacheMatrix creates a special object, which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse of the matrix
## - get the value of the inverse of the matrix
##

makeCacheMatrix 	<- function(x = matrix()) 
{
	cachedInverse 	<- NULL
	set 			<- function(y) 
	{
	x 			<<- y
	cachedInverse 	<<- NULL
	}
	get 			<- function() x
	setSolve 		<- function(solve) cachedInverse <<- solve
	getSolve 		<- function() cachedInverse
	list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

##
## cacheSolve: 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
##
## This function assumes that the matrix supplied is always invertible

cacheSolve <- function(x, ...) 
{
	invFunc 		<- x$getSolve()
	if (!is.null(invFunc)) 
	{
				message("getting cached data")
				return(invFunc)
	}
	data 			<- x$get()
	invFunc 		<- solve(data, ...)
	x$setSolve(invFunc)
	invFunc
}