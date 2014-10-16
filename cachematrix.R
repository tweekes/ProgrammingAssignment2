## Provides a set of functions for running and caching matrix inversion rusults.
## makeCacheMatrix() - implements the caching mechanism. 
## casheSolve() - returns the inverted matrix, if the matrix was inverted previously
##                then this is returned. Otherwise, solve() is called to invert the matrix
##                data and results are cached.   


## makeCacheMatrix provide a set of nested functions for caching original and inverted
## matrix data. The following nested functions are available on an instantiated instance
## of the function: 
##    set(x)
##    get(), 
##    setInverted(i)
##    getInverted()

## Note, x must be a square and invertible matrix. 

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL  # i will hold the inverted matrix. 
  
  set <- function(newX) {
    x <<-newX
    i <<-NULL  ## Because new data,x, we must clear i    
  }
  
  get <- function() {x}
  
  setInverted <- function(iNew) {i<<-iNew}
  
  getInverted <-function() {i}
  
  list(set = set, 
       get = get,
       setInverted = setInverted,
       getInverted = getInverted)

}

## Inverts the 'originai' matrix data cached in x. If inverted matrix 
## is not in the cache then the original data is inverted cached and then 
## returned.

cacheSolve <- function(x, ...) {
  
  i <-x$getInverted()
  
  if (!is.null(i)) {
    message("Getting cached data")
    return(i)
  }  

  data<- x$get()
  i <- solve(data,...)
  x$setInverted(i)
  i ## Last expression is retured by function. 
}