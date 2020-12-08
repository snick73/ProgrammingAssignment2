## This assignment is to solve the inverse of a matrix caching the 
# result within a lexical scope of a function

makeCacheMatrix <- function(x = matrix()) {
# Example input: Insert matrix x<-matrix(rnorm(64),8,8)
## To check cached values: 
# xMat<-makeCacheMatrix(x)  # Run the function
# parent.env(xMat$getenv())$m  # Check the cached mean
# environment(xMat$getmean)  # refer to environment of "m"
m<-NULL  # assigns NULL to a variable within the current environment 
evn <- environment()  # Save environment
y<-NULL 

setmatrix<-function(y){  # Set matrix value
	x<<-y  # cache the matrix - assigns value y from parent environment
	m<<-NULL # search through parent environments for an existing definition of the variable and set to NULL
	}
  
getmatrix<-function() x  
setinverse<-function(solve) m<<- solve  
getinverse<-function() m  
getenv<- function() environment()

list (setmatrix=setmatrix, getmatrix = getmatrix,
setinverse = setinverse,
getinverse = getinverse,
getenv = getenv)

}


## The function "cacheSolve" returns the inverse of the matrix that is 
# returned by makeCacheMatrix function, e.g. xMat$getmatrix()


cacheSolve <- function(xMat= m(), ...) {
	m <- xMat$getinverse() # if an inverse has already been calculated this gets it
	if(!is.null(m)){ # check to see if cacheSolve has been run before
		if(xMat$setmatrix() == xMat$getmatrix()) { # check that matrix hasn't changed, and if it hasn't, sends a text message and returns the cached matrix
    	message("getting cached data")
    	matrix<-xMat$get()
    	m<-solve(matrix, ...)
    	xMat$setmatrix(m)
    	return(m) 
    	}
    	# otherwise 
    	y <- xMat$getmatrix() # run the getmatrix function to get the value of the input matrix
    	xMat$setmatrix(y) # run the setmatrix function on the input matrix to cache it
    	m <- solve(y, ...) # compute the value of the inverse of the input matrix
    	xMat$setinverse(m) # run the setinverse function on the inverse to cache the inverse
    	m # return the inverse
    	}
    	# End
}
