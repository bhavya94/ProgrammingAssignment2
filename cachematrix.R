## The aim of the given program is to take in a Matrix , say m, as an argument to generate a Cache Matrix
## which is used to store the recurring values of computations used in inversing the matrix. And then solve 
## the cached matrix to finally derieve the inverse of the matrix.

## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1.set the value of the vector ;
## 2.get the value of the vector ;
## 3.set the value of the cached matrix ; and
## 4.get the value of the cached matrix

makeCacheMatrix <- function(x = matrix())
{
  	m<-NULL
  	
## Set the value of vector
	set<-function(y) 
	{
  		x<<-y
  		m<<-NULL
	}
	
## get the value of vector
	get<-function() x 
	
## set the value of cached matrix
	setmatrix<-function(solve) m<<- solve  
	
## get the value of cached matrix
	getmatrix<-function() m 
	
## Generate the list of all computed variables.	
	list(set=set, get=get,
   	setmatrix=setmatrix,
   	getmatrix=getmatrix) 
}




## The following function calculates the inverse of the special "vector" created with the above function. 
## However, it first checks to see if the inverse of a particular element has already been calculated. 
## If so, it gets the computed value from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse of matrix element in the cache via the setmean 
## function

cacheSolve <- function(x=matrix(), ...)
{
    	m<-x$getmatrix()
    	
## Checking the cache.
	if(!is.null(m))
	{
      		message("getting cached data")
      		return(m)
    	}
## The computation part.
	matrix<-x$get()
    	m<-solve(matrix, ...)
    	x$setmatrix(m)
    	m
}

## End of code.
