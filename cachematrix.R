## MAKECACHEMATRIX
## makeCacheMatrix creates a "special matrix", which is really a list containing a function to
##
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse matrix
## 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  
  getinverse <- function() i
  
  list( set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
  )
  
}

## CACHESOLVE
## cacheSolve calculates the inverse matrix of the "special matrix" 
## created with makeCacheMatrix.
## However, it first checks to see if the inverse matrix has already 
## been calculated. If so, it gets the inverse from the cache and 
## skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the 
## value of the inverse in the cache via the setinverse function.
##
## To be more generic, this function does not use the solve function,
## which is useful only with square matrix, but instead uses the ginv()
## function from the MASS module, which using the Moore-Penrose generalized inverse
## can calculate the inverse of a rectangular matrix too.

library(MASS)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  ## if the matrix is always square, we could use
  ## i <- solve(data)
  ## but in the general case, supposing data is always invertible,
  ## I could use the function ginv() from the MASS module
  i <- ginv(data)
  x$setinverse(i)
  i
}

## TEST
## To test the correctness of the function I used a simple example from
## MathWorld (http://www.mathwords.com/i/inverse_of_a_matrix.htm)
## You can run this code to test the function
##
## q <- matrix(c(4,3,3,2),nrow=2,ncol=2,byrow=TRUE)
## c <- makeCacheMatrix(q)
## s <- cacheSolve(c)
## q%*%s
##
## It should give you something similar
##
##      [,1]  [,2]
##[1,]    1     0
##[2,]    0     1
##
