##  Returns the inverse of a matrix.  If the inverse has been calculated,
##  the inverse is cached for later retrieval.
##
##  The inverse is only calculated if the original matrix x
##  has changed and the inverse needs ot be recalculated


makeCacheMatrix <- function(original_matrix = matrix()) {
#**************************************************
#  Creates a list containing functions which 
#    1) sets the original matrix
#    2) gets the original matrix
#    3) sets the matrix inverse
#    4) gets the matrix inverse
#**************************************************
  
  inverse <- NULL
  
  set <- function(y) {
    original_matrix <<- y
    inverse <<- NULL
  }
  
  get <- function() original_matrix
  
  setinverse <- function(inverse_in)  inverse <<- inverse_in
  
  getinverse <- function() inverse
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}



cacheSolve <- function(original_matrix, ...) {
##############################################
##  Return a matrix that is the inverse of 'original_matrix'
##  But utilize a cache to only calculate the inverse if the 
##############################################  

  inverse <- original_matrix$getinverse()

  if (!is.null(inverse)) {
    
    message("getting cached data")
    return(inverse)
  
  }

  matx <- original_matrix$get()

  inverse <- solve(matx)

  original_matrix$setinverse(inverse)

  inverse
  
}
