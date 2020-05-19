## These two functions work together. makeCacheMatrix takes a matrix
## as input, creates an empty variable to hold the inverse, and 
## outputs a list of functions for use by cacheSolve. cacheSolve
## retrieves the inverse object, checks and returns the value
## if it holds an inverse, otherwise retrieve the initial matrix 
## input and calculates the inverse, storing the result in the 
## original environment.

## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(M = matrix()) {
      Mi <- NULL #empty object to hold the inverse matrix
      setM <- function(newMatrix){ #setter that takes new user input
            M <<- newMatrix       #after object mcm is created
            Mi <<- NULL
      }
      getM <- function() M #Returns the user input matrix
      setMi <- function(inv) Mi <<- inv
      getMi <- function() Mi
      list(setM = setM, getM = getM, 
           setMi = setMi, getMi = getMi)
}


## This function takes object "mcm" as an argument, collects the
## inverse object from above, checks if it is already cached,
## returns it if it is, otherwise retrieves matrix M and
## calculates, caches, and returns its inverse

cacheSolve <- function(mcm) {
      Mi <- mcm$getMi() #retrieve object Mi from mcm environment
      if(!is.null(Mi)) {  #check if inverse exists already
            message("retrieving cached inverse")
            return(Mi)
      }
      mat <- mcm$getM()
      Mi <- solve(mat)
      mcm$setMi(Mi) #set the new inverse to Mi in mcm environment
      Mi ## Return a matrix that is the inverse of 'M'
}
