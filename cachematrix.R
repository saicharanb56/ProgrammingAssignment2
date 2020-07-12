## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function returns a list containing elements which are the names of
## functions that set the value of the matrix, get the value of the matrix, 
## set the inverse of the matrix, and get the inverse of the matrix, in that
## specific order.

makeCacheMatrix <- function(x = matrix()) {

	a <- NULL
	set <- function(y) {
		x <<- y
		a <<- NULL
	}

	get <- function() x
	setInv <- function(inv) a <<- inv
	getInv <- function() a
	
	list(set = set, get = get, getInv = getInv, setInv = setInv)
}


## Write a short comment describing this function

## This function calculates the inverse of the output from the makeCacheMatrix
## function. It first checks whether the inverse has been calculated. If so,
## it gets the inverse from the cache and saves time on computation. If not, 
## it calculates the inverse and stores the value in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	a <- x$getInv()
	if(!is.null(a)) {
	
		message("please find cached data below")
		return(a)
	}

	data <- x$get()
	a <- solve(data, ...)
	x$setInv(a)
	a
}
