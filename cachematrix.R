## Provides a set of functions for running and caching matrix inversion rusults.
## makeCacheMatrix() - implements the caching mechanism. 
## casheSolve() - returns the inverted matrix, if the matrix was inverted previously
##                then this is returned. Otherwise, solve() is called to invert the matrix
##                data, the results are cached, and the returned.   


## makeCacheMatrix provides a set of nested functions for caching original and inverted
## matrix data. The following nested functions are available on an instantiated instance
## of the function:
##    set(x) - set new orginal data on instantiated instances.
##    get(), - get the orginal data.
##    setInverted(i) - set inverted data.
##    getInverted() - get already cached data.

## Note, x must be a square and invertible matrix. 

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL  # i will hold the inverted matrix. 
  
  set <- function(newX) {
    x <<-newX
    i <<-NULL  ## Because there is new orginal data, x, we must clear i.     
  }
  
  get <- function() {x}
  
  setInverted <- function(iNew) {i<<-iNew}
  
  getInverted <-function() {i}
  
  list(set = set, 
       get = get,
       setInverted = setInverted,
       getInverted = getInverted)

}

## Inverts the 'original' matrix data cached in x. If the inverted matrix 
## is not in the cache then the original data is inverted, cached and then 
## returned.

cacheSolve <- function(x, ...) {
  
  i <-x$getInverted()
  
  if (!is.null(i)) {
    message("Getting cached insverted data")
    return(i)
  }  

  data<- x$get()
  i <- solve(data,...)
  x$setInverted(i)
  i ## Last expression is retured by function. 
}


##########################################################
# Test Cases
##########################################################

# m <- matrix(c(4,2,7,6),2,2)
# x <- makeCacheMatrix(m)
# n <- cacheSolve(x)
# x$set(n)
# o <- cacheSolve(x)
#
#       [,1] [,2] 	       [,1] [,2]             [,1] [,2]                   
#	[1,]  4    7     =>	[1,]  0.6 -0.7  =>  [1,]    4    7	             
#	[2,]  2    6	 	    [2,] -0.2  0.4      [2,]    2    6	    
#
#       'm'                 'n'                   'o'     

