#----------------------------------------------------------------------------#
#Programming Assignment 2: Lexical Scoping
#----------------------------------------------------------------------------#

#===========================================================================================
# Codes adapted from "Example: Caching the Mean of a Vector" given in assignment description
#===========================================================================================

# Define the MakeCacheMatrix function
makeCacheMatrix <- function(x) 
{
  #Set the inverse of the matrix to null initially
  inverse <- NULL
  
  #Constructor of object - set the parameter x i.e. the matrix and initial inverse to be null
  set <- function(y) 
  {
    x <<- y
    inverse <<- NULL
  }
  
  #Function to retrieve the matrx
  getmatrix <- function() x
  
  #Function to set the inverse of the matrix
  setinverse <- function(p_inverse) inverse <<- p_inverse
  
  #Function to retrieve the inverse of the matrix
  getinverse <- function() inverse
  
  #Create the list of functions to use in the object
  list(set = set, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Define the function 
cacheSolve <- function(x, ...) 
{
  
  #Retrieve the inverse matrix from object
  inv_m <- x$getinverse()
  
  #If null, shows that the inverse has not been created, if not null, shows that it
  #has been created so no need to create, just return the inverse matrix
  if(!is.null(inv_m)) 
  {
    print("Inverse matrix found, retrieving it now!")
    return(inv_m)
  }
  
  print("Inverse matrix not found, got to calculate it")
  
  m <- x$getmatrix()
  inv_m <- solve(m, ...)
  x$setinverse(inv_m)
  
  print("Inverse matrix value set")
  return (inv_m) 
}

#-------------------------------------------------------------------------------------
# Test whether both functions work
#-------------------------------------------------------------------------------------

# Round 1: Create the matrix and retieve the inverse
# Create a random 2x2 matrix
set.seed(3001)
r = rnorm(4)
test_matrix = matrix(r, nrow=2, ncol=2)
print(test_matrix)

# Create an object on the test matrix and call the cache solve function
temp = makeCacheMatrix(test_matrix)
inv_m <-cacheSolve(temp)
print(inv_m)

# Round 2: Retrieve the inverse second time, there should be a "Inverse matrix found...
# printed in the console
inv_m <- cacheSolve(temp)
print(inv_m)


#-------------------------------------------------------------------------------------
# NOTE: This is not part of assigment but sees whether the individual functions work
# in the makeCacheMatrix
#-------------------------------------------------------------------------------------

# Create a random 10x10 matrix
set.seed(3000)
r = rnorm(100)
test_matrix = matrix(r, nrow=10, ncol=10)

# Create an object of the matrix using the MakeCacheMatrix function
temp = makeCacheMatrix(test_matrix)
getval = temp$getmatrix()

# If the same matrix as test_matrix is printed, function works!
print(getval) #Does it return same value as test_matrix?

# Retrieve the inverse of the matrix which should return NULL since the inverse not calculated
getinv = temp$getinverse() 
print(getinv) #Does the inverse return a NULL

# Calculate an inverse of the matrix and set the matrix and retrieve again, it should show up
calinv = solve(getval)
temp$setinverse(calinv)
getinv = temp$getinverse() 
print(getinv) #Does the inverse return the same value as calinv