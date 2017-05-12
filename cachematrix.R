## Put comments here that give an overall description of what your
## functions do

## Funcion makeCaheMatrix
# returns a list of functions
# $setMatrix set(store) the original matrix (makeCaheMatrix argument)
# $getMatrix return stored matrix
# $setInvMatrix set(store) the inverse matrix (argument)
# $getInvMatrix return stored inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  InvMatrix <- NULL    # init
  setMatrix <- function(y) {
    x <<- y
    InvMatrix <<- NULL
  }
  getMatrix <- function() x
  setInvMatrix <- function(inv) InvMatrix <<- inv
  getInvMatrix <- function() InvMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInvMatrix= setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' (argument)

  InvMatrix <- x$getInvMatrix()
  if(!is.null(InvMatrix)) {   # inverse matrix still stored?
    message("getting cached data")
    return(InvMatrix) # still stored (not NULL)
  }
  myMatrix <- x$getMatrix() # get Matrix using fn getMatrix
  InvMatrix <- solve(myMatrix, ...) #inverse 
  x$setInvMatrix(InvMatrix) # Set resulte in cache for the next time
  InvMatrix # return value
} 
  
  
########################   TEST TEST TEST 

#aVector <- makeCacheMatrix(cbind(c(2, 0, 0), c(0, 2, 0), c(0, 0, 2)))
#aVector$getMatrix()               # retrieve the value of x
#aVector$getInvMatrix()           # retrieve the value of inverse , which should be NULL ==> OK 
#aVector <- makeCacheMatrix(cbind(c(3, 0, 0), c(0, 3, 0), c(0, 0, 3)))        # reset value with a new vector
#cacheSolve(aVector)          # inverse last makeCacheMatrix
#aVector$getInvMatrix()           # retrieve it directly, now that it has been cached  ==> ok 
