#The makeCacheMatrix function creates a list that tracks all of the matrixes
#and potential matrix solutions that have been input into the system

makeCacheMatrix <- function(x = matrix()) {
  #This stores the inverse of the matrix as a null value
  inv <- NULL
  
  #The function stores the y value as x, sets the inverse to null
  #Impacts retrieval later on
  
  s.mat <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #This function will return the original matrix input
  
  g.mat <- function() {
    x
  }
  
  #This function will store the "solved" (inverted) matrix in the set
  
  s.inver <- function(calc) {
    inv <<- calc
  }
  
  #This function will list the calculated inverse.
  
  g.inver <- function() {
    inv
  }
  
  #Function ultimately creates searchable list of matrixes with 
  #respective values stores for the solved parts.
  
  list(s.mat = s.mat, g.mat = g.mat,
       s.inver = s.inver,
       g.inver = g.inver)
}

#cacheSolve either retrieves the calculated inverse of a matrix from
#the data repository maintained by makeCacheMatrix or calculates a
#new inverse solution and stores it in the repository/list

cacheSolve <- function(x, ...) {
  #searches for the inverse using the get inverse function, stores it
  
  inv <- x$g.inver()
  
  #tests to see if value is available (not null)
  #then produces the stored value for the inverse/solution
  
  if(!is.null(inv)) {
    message("Retrieving cache calculation")
    return(inv)
  }
  
  #if the original value is null this calculates the inverse after
  #using the g.mat function to take in the matrix information
  
  chol.mat <- x$g.mat()
  inv <- solve(chol.mat)
  
  #sets the value of the inverse within the list and returns the inv
  
  x$s.mat(inv)
  inv
}