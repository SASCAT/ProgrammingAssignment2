## R Programming Assignment 2
##
## Jeff Tomlinsn - August 2014
##
## Functions to invert a square matrix.  
## makeCacheMatrix initializes the data structure and functions required for
## the cacheSolve function to invert the matrix.  If the matrix has already 
## been inverted by cacheSolve, then the results of the previous inversion
## are returned.
##
## Usage :
## 
## # 20x20 matrix to invert.
## mat <- matrix(rnorm(400),20,20)
## # Initialize the "special" matrix.
## specmat <- makeCacheMatrix(mat)
## # Invoke cacheSolve to invert the matrix
## cacheSolve(specmat)
## # Second invocation will return the previous result for specmat
## cacheSolve(specmat)

## This function takes an input square vector and sets up a list with a 
## set of functions that allows the matrix inverse to be calculated by the
## cacheSolve function if the inverse has not already been calculated. 
## If it has been calculated previously, then the cached inverted matrix 
## is returned instead of re-calculating the inverse.
##
## result - stores the inverse of the matrix.  It is initialized to NULL. 
##       cacheSolve will store the inverse of the matrix in this.
##
## The returned list has 4 elements which are functions with the same name
## as the labels for the elements in the list (for simplicity)
##
## initData function - initialises the data structure. x is a copy of the 
##       original matrix and result will be used for the inverse.
## getData function - returns the input matrix.
## calcResult function - Calculates the inverse of the input matrix and 
##       returns the result in result.
## getResult function - returns the inverse of the matrix, if it has already
##       been calculated.
##
## makeCacheMatrix returns the list of constructor and access functions
## for the "special" matrix
makeCacheMatrix <- function(x = matrix()) {
  
  result <- NULL
  # Function to initialize the "special" matrix data-structure
  # Copy of the source matrix and a NULL result
  initData <- function(y) {
    x <<- y
    result <<- NULL
  }
  
  # getData returns the data for the source matrix
  getData <- function() {
    x
  }

  # calcResult invokes the solve function to invert the matrix and saves the 
  # inverted matrix in result.
  calcResult <- function() {
    data <- getData()
    result <<- solve(data)
  }
  
  # getResult returns the result of the previously inverted matrix.
  getResult <- function() {
    result
  }
  
  # Initialize the "special" matrix.
  initData(x)
  
  # Return the list of fucntions  ... 
  list(initData=initData, getData=getData, calcResult=calcResult, getResult=getResult)
}


## This function returns the inverse of the supplied "special" matrix 
## Created by makeCacheMatrix.  
## 
## The parameter x is the "special" matrix created by makeCacheMatrix,
## which is actually a list of functions.
##
## Action:
## If the result of a previous call to invert the matrix is present, 
## (getResult returns a value that is not NULL) then that matrix is 
## returned.  Otherwise the solve function is called to invert the 
## matrix and the returned inverted matrix is stored via the calcResult
## function. 
cacheSolve <- function(x, ...) {
  
  # Get previous result (NULL is no previous result)  
  result <- x$getResult()
  
  # Just return the previous result if it is present.
  if(!is.null(result)) {
    message("Getting cached matrix ...")
    return(result)
  }
  
  # No previous result, so invoke calcResult to invert the matrix and 
  # save the result.
  result <- x$calcResult()
  
  # ... return the inverted matrix.
  result
}
