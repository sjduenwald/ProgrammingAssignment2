## Function to support the lexical scoping of a inverse matrix calculation
## Input a square matrix in makeCachMatrix

makeCacheMatrix = function(ma = matrix()){
  m <- NULL #declare m and set to null 
  set <- function(y) {  #set the values for ma and m to exist in the parent environment
    ma <<- y
    m <<- NULL
  }
  get <- function() ma #returns the input matrix
  setmatrix <- function(inverse) m <<- inverse #function cache the inverse matrix
  getmatrix <- function() m #pull the cached inverse matrix
  #return the declared functions as a list
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Evaluates if a inverse matrix has been calculated for an input
## If the inverse matrix has not been calculated if calculates and caches the inverse
## If the inverse matrix has been calcuated returns the cached inverse matrix

cacheMatrix = function(x, ...){
  m <- x$getmatrix()    #get the inverse matrix value
  if(!is.null(m)) {     #is there already a stored inverse matrix?
    message("getting cached data")  #inform user if returning cached inverse matrix
    return(m) #return inverse matrix and exit function
  }
  data <- x$get() #pull inputed matrix
  m <-solve(data, ...)  #calculate inverse matrix
  x$setmatrix(m)  #cache the inverse matrix
  m #return inverse matrix
}

