#Creates a special matrix, which is a list containing a function to
# 1 - set the value of the matrix
# 2 - get the value of the matrix
# 3 - set the value of the inverse of the matrix
# 4 - get the value of the inverse of the matrix
  
makeCachematrix <- function(x = matrix()) {             # defines matrix as 'x'
  invmatrix <- NULL                                     # the inverse matrix defined as 'invmatrix' is empty
  set <- function(y) {                                  #sets the function to matrix 'x'
    x <<- y                                              
    invmatrix <<- NULL
  }
  get <- function() x                                   #gets the value of the matrix 'x' that will be inverted
  setinverse <- function(inverse) invmatrix <<- inverse #sets the value of the inverted matrix 'invmatrix'
  getinverse <- function() invmatrix                    #gets the value of the inverted matrix 'invmatrix'
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getinverse()  #gets the inverse from getinverse
  if(!is.null(invmatrix)) { # checks to see if 'invmatrix' has been cached, 
    message("getting cached data") # if 'invmatrix' is present, return 'getting cached data'
    return(invmatrix)
  }
  data <- x$get()       # if not present, gets the matrix 'x'
  invmatrix <- solve(data, ...) #the solve function calculates the inverse of the matrix 'x'
  x$setinverse(invmatrix)     #sets the inverse of the matrix 'x' as 'invmatrix'
  invmatrix                   #return the inverted matrix
}
