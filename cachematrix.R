## A collection of functions to cache potentially time-consuming
## computations and a function to demostrate how the caching functions
## are used. The example function will assigned to the variable 
## cacheSolve and will return the inverse of a matrix

 
#
# Create a datastructure that can set/get the matrix and its inverse.
#
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(m) {
    x <<- m
  }
  get <- function() x
  setInverseMatrix <- function(matrix) inverseMatrix <<- matrix
  getInverseMatrix <- function() inverseMatrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

#
# Create a datastructure that will set/get a vector and/or mean
#
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
  setmean = setmean,
  getmean = getmean)
}

#
# 1.  If the mean has been set then the mean
# 2.  If the mean has not been set then calculate the mean, store it and return it.
#
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
  message("getting cached data")
  return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## An exmple function show how to the makeCacheMatrix function.
## It will accept a matrix and return the inverse of a matrix. 
cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  matrix <- x$get()
  inverseMatrix <- solve(matrix)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}

#########################################################################

##
## Study the makeMaxtrix
##
testCase3 <- function() {
  testCase3 <- makeCacheMatrix()
  print(testCase3)
  
  matrixList <- testCase2()
  
  element1 <- matrixList[1]
  element2 <- matrixList[2]
  
  #print(element1)
  #print(element2)


  testCase3$set(element1)
  testCase3$setInverseMatrix(element2)
  print(testCase3$get())
  print(testCase3$getInverseMatrix())
  
  
}

##
## Study the solve function 
##
testCase2 <- function() {
  sourceMatrix <- matrix(c(4,3,3,2), nrow = 2, ncol = 2)
  inverseMatrix <- solve(sourceMatrix)
  identifyMatrix <- sourceMatrix %*% inverseMatrix

  #print(sourceMatrix)
  #print(inverseMatrix)
  #print(identifyMatrix)
  
  matrixCollection <- list(sourceMatrix, inverseMatrix)
  
}

##
## Study the output of make vector
## 
testCase1 <- function() {
  
  testInput <- c(10:20)
   
  testCase1 <- makeVector()
  testCase1$set( testInput )
  testCase1$setmean( mean(testInput))
  print(testCase1)
  #print("-----------------------")
  #print(str(testCase1$get()))
  #print("+++++++++++++++++++++++")
  #print(str(testCase1$getmean()))
  #print("-----------------------")
}


