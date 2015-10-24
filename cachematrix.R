## The purpose of this file is to optimize a matrix
## inversion by doing the inversion only one and caching
## the result. 

##
## Create of a list of functions that the
## developer can set and get the matrix and
## its inverse.
##
## Each time this function is called it 
## creates different setter/getter so 
## there can be different matrix caches 
## with different values
##
makeCacheMatrix <- function() {
  
  #
  # Store the matrix in the globlal environment
  # so the variable exist when the function has
  # finished executing and goes out of scope.
  # 
  set <- function(m) 
  {
    x <<- m
    inverseMatrix <<- NULL
  }
  
  #
  # Retrieves the matrix from the global environment
  #
  get <- function() 
  {
    x
  }

  #
  #
  # Store the inverse matrix in the globlal environment
  # so the variable exist when the function has
  # finished executing and goes out of scope.
  # 
  # 
  setInverseMatrix <- function(matrix) 
  {
    inverseMatrix <<- matrix
  }
  
  #
  # Retrieve the inverse matrix from the global environment.
  #
  getInverseMatrix <- function() 
  {
    inverseMatrix
  }
  
  #
  # Creates a list of name, functions ( name = set, function= set)
  # that the developer can call by accessing them using the variable$name
  # 
  # The list 
  list(set = set, 
       get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

##
## A fucntion that will return the inverse of the 
## matrix
##
## If the inverse has been stored in the cacheMatrix 
## the inverse will be retruned
##
## If the inverse has not been store in the cacheMatrix
## then the inverse will be calculated, stored in x (
## fromal parameter ) and retured to the user.
##
cacheSolve <- function(x, ...) 
{
  #
  # If the inverse maxtrix exists
  #   1. return the inverse matrix 
  #
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)) 
  {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  #
  # If the inverse matrix does not exist
  #    1. calculate the inverse matrix
  #    2. store the matrix so that if save variable 
  #       is used then it does need to be calculated.
  #    3. return the inverse matrix.
  #
  matrix <- x$get()
  inverseMatrix <- solve(matrix)
  x$setInverseMatrix(inverseMatrix)
  
  inverseMatrix
}

#########################################################################
## Functions used to understand the assignment                         ##
#########################################################################

##
## A routine to verify that the makeCacheMatrix is working
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


