## The functions bellow will implement a methodology to gets a matrix inverse of
##a given matrix in a special form that permits to cache a recent matrix inverse matrix given 
## inside a previously given data structure . 

##This fuction gets the matrix that we wish to get the inverse and returns a 
##data structure that is expected by the cacheSolve function
##that is the fuction that really will give the matrix inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get<- function()x
  setInverse <- function(inverseMat)inverseMatrix <<- inverseMat
  getInverse<- function() inverseMatrix
  returnValue <-list(set = set,
                     get = get,
                    setInverse = setInverse,
                    getInverse = getInverse)
  return(returnValue)
  

}


## This function gets a data structure given by the makeCacheMatrix function
## and give the inverse of the matrix inside the previously given data structure.
## If the given data structure already has the matrix inverse inside it the fuction
## will return the previously calculated inverse and will notify it through a message.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse()
    
    if(!is.null(inverseMatrix)){
      message("getting cache data")
      return(inverseMatrix)
    }
    
    matrixToInvert <- x$get()
    
    inverseMatrix <- solve(matrixToInvert)
    x$setInverse(inverseMatrix)
    
    return(inverseMatrix)
}
