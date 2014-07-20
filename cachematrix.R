## makeCacheMatrix is a "contructor" funtion which has a list of functions
## to get the Matrix value, set the Matrix value and get/set the matrix 
## inverse values. When a matrix is passed to this function, it associates these 
## functions to the matrix. And when we pass this list to CacheSolve function, 
## it uses these functions to check if the cached value is available or not. If not 
## it uses Solve function to calculate the inverse and pass it to the SetMatrixInverse
## function.  <<- operator which can be used to assign a value to an object in an 
## environment that is different from the current environment

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	mi<-NULL
	set<-function(y)
	{
		x<<-y
		mi<<-NULL
	}
	get<-function() x
	setMatrixInverse<-function(matrixInverse)mi<<-matrixInverse 
	getMatrixInverse<-function()mi
	
	list(set=set,get=get,setMatrixInverse=setMatrixInverse,
		getMatrixInverse=getMatrixInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m<-x$getMatrixInverse()
	if(!is.null(m))
	{
		message("Getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data)
	x$setMatrixInverse(m)
	m
}
