## These set of functions creates and manipulates special 'matrix' objects, mainly to
## be able to cache the previously solved/calculated inverse of matrices.

## creates a special 'matrix' object which contains a maxtrix as well as its inverse.
## Comes with getter and setter routines for the maxtrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) i <<- inverse
	getInv <- function() i
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Takes a special 'matrix' object, computes the inverse of the matrix within and stores it
## (if not already done). Otherwise it merely returns the pre-computed inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	i <- solve(x$get())
	x$setInv(i)
	i
}


## EOF - test
