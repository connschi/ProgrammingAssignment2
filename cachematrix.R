# The purpose of this file is to optimize is to do 
# the inversion of matrix only once.  The operation 
# will done by creating a Matrix type that can cache 
# the inverse of itself.

#
# Create a datastructure that can set/get the matrix 
# and its inverse.
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
# A fucntion that will return the inverse of the 
# matrix
#
# If the inverse has been stored in the cacheMatrix 
# the inverse will be retruned
#
# If the inverse has not been store in the cacheMatrix
# then the inverse will be calculated, store in the 
# cacheMatrix and retured to the user.
#
#
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
## Functions used to understand the assignment                         ##
#########################################################################

##
## A routine to verify that the 
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
  #print(testCase3$get())
  #print(testCase3$getInverseMatrix())
  
  
}

##
## Study the solve function to invert a matrix.  Note that
## I multiply them together to get the identity matrix.
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
## Study the output of makeVector to learn how to set/get a vector/mean.
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

##########################################
## Original Source Code provided by     ##
## the professors.                      ##
##########################################

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
# 2.  If the mean has not been set then calculate the mean, 
#     store it and return it.
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


