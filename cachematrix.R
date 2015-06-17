## Author: Hong Thai Koh (17 June 2015) 
## This is the submission for Assignment 2, R Programming. 
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. 
## Below are 2 functions, makeCacheMatrix and cacheSolve that cache the inverse of a matrix.  

## This function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse.
## It creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## inverse the matrix
## get the inverse of matrix 
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	
        set <- function(y){
		x <<- y
		m <<- NULL
	}
	
        get <- function() x
	
        setmatrix <- function(solve) m <<- solve
	
        getmatrix <- function() m
	
        list(set=set, get=get,
	        setmatrix=setmatrix,
	        getmatrix=getmatrix)
}

## The following function inverses special "matrix" created with the above function. 
## However, it first checks to see if the matrix has already been inverted. 
## If so, it gets the inverted matrix from the cache and skips the inversion. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache 
## via the setmean function
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getmatrix()
        
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
	
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
