## This function returns the inverse of a matrix. If the inverse
## of the matrix already exists, it will return the inverse 
## directly without calculating. Otherwise it will calculate the
## inverse of the matrix and store it in the cache.


## Return the list which contains functions as follows: 1.set the 
## value of the matrix 2.get the value of the matrix 3.set the 
## value of the inverse 4.get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
	    inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inver) inverse <<- inver
	getinverse <- function() inverse
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## If the inverseof the matrix already exists, it will return the
## inverse directly without calculating. Otherwise it will 
## calculate the inverse of the matrix and store it in the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if (!is.null(inverse)){
        	message("getting cached data")
        	return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        return(inverse)       
}
