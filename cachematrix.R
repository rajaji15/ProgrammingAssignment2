## MatrixInversion- Computing the matrix inversion is very costly operation and 
## The functions  below functions  leverages the scoping rules (lexical scoping) of
## R Programming language to preserve the state inside of an R Object and utilise the values 
## from cache when it is required again instead of computing again and again.


## makeCacheMatrix creates /returns a list containing functions to 
##   1. set the matrix
##   2. get the matrix
##   3. set the inverse
##   4. get the inverse
##   input is a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  setMatrix <- function(y)
  {
    ## ' <<-'  operator used to assign the value of an object in an environment 
    ## different from the current environment
    
    x<<-y
    inv<<-NULL
  }
  getMatrix <- function() x
  
  setInvMatrix <- function(inverse)
    inv<<- inverse
  
  getInvMatrix <- function() inv
  
  list(setMatrix=setMatrix,getMatrix=getMatrix,setInvMatrix=setInvMatrix,getInvMatrix=getInvMatrix)

}


## cacheSolve function returns the inversion of matrix. The function first will check
## if the inverse is already computed and available in cache. if yes it skips the computation 
## and returns the value from cache. if not , it computes the inverse value and set the value
## in to the cache through setInvMatrix function.

cacheSolve <- function(x, ...) {
  
  
        ## Return a matrix that is the inverse of 'x'

  inv=x$getInvMatrix()
  if(!is.null(inv)) {
    message("get the inverse value from cache")
    return(inv)
  }
  data <-x$getMatrix()
  inv <-solve(data, ...)
  x$setInvMatrix(inv) 
  inv
}