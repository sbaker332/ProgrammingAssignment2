## This set of functions will calculate and return the inverse of a matrix
## to increase performance the inversion will be stored in an environment 
## variable so that it does not have to be calculated on each call
## but instead will be calculated once and then returned from memory

## makeCacheMatrix will take any matrix and return a vector
## that contains functions for getting and setting the original matrix
## and the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## first set the inverse matrix to NULL so that it is empty
  invMatrix <- NULL
  
  ## create a function for set for setting the original matrix
  set <- function (y) {
    
    ##store the orignal matrix in the variable x
    x <<- y
    
    ## set the inverse matrix to null as the orginal matrix
    ## has been changed so the inverse will need to be recalculated
    invMatrix <<- NULL
  }
  
  ## create a function to return the original matrix
  get <- function() x
  
  ## create a function to store the inverse matrix in the 
  ## variable invMatrix
  setInvMatrix <- function(inv) invMatrix <<- inv
  
  ## create a function to return the inverse matrix
  getInvMatrix <- function() invMatrix
  
  ## create the vector that get's returned that contains the named
  ## members representing the different functions that have been previously
  ## defined to either get or set the corresponding matrix 
  list(set=set, get=get, setmatrix=setInvMatrix, getmatrix=getInvMatrix)
  
}


## This function will take a matrix and return the inverse of that matrix
## from the cache is if the inverse exists in the cache, otherwise 
## we have to calculate it and then store it in the cache before returning it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## first get the inverse for the original matrix as defined in the matrix
  inv <- x$getmatrix()
  
  ## now we have to check to see if the inverse is null
  if(!is.null(inv)){
    
    ## since the inverse is not NULL we can return the value
    return (inv)
  }
  
  ## the inverse that was returned was NULL so we now have to 
  ## calculate the inverse and then store that in the cache 
  ## for future reference
  
  ## get the original matrix
  origMatrix <- x$get()
  
  ## calculate the inverse of the original matrix using the 
  ## solve function in R
  inv <- solve(origMatrix, ...)
  
  ## now that the inverse has been calculated we have to store
  ## it in the cache for future reference
  x$setmatrix(inv)
  
  ## now return the inverse
  return(inv)
  
}
