## The function makeCacheMatrix takes a matrix as it's argument ## and adds
## the functionality to set and get itself and it's inverse.


makeCacheMatrix <- function(x = matrix()) {

	mt <- NULL
	set <- function(d){
		x <<- d
		mt <<- NULL
	}
	get <- function() x

	set_inverse <- function(invmat) mt <<- invmat
	get_inverse <- function() mt
	
	list(set=set, get=get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function acts as an agent to the special matrix object ## tasked with returning 
## the cached version of the inverse whenever available.

cacheSolve <- function(x, ...) {
        
	mt <- x$get_inverse()
	if(!is.null(mt)){
		message("getting cached version of inverse")
		return(mt)
	}
	data <- x$get()
	invmat <- solve(data, ...)
	x$set_inverse(invmat)
	invmat
}