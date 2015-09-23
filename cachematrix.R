## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {
  
  ##Initialize InversedMatrix variable
  inversedMatrix <- NULL
  
  ##define set-function
  set <- function(y) {
    
    ##set variables to enclosing environment
    ## (<<- operator)
    
    x <<- y
    
    ##Initialize InversedMatrix variable
    inversedMatrix <<- NULL
  }
  
  ##define get-function
  get <- function() x
  
  ##define setinverse-function
  ##function assigns value to enclosing environment
  setinverse <- function(solve) inversedMatrix <<- solve
  
  ##define getinverse-function
  getinverse <- function() inversedMatrix
  
  ##return a list containing object's members
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ##call getinverse
  ##this returns variable inversedMatrix from object x
  m <- x$getinverse()
  if(!is.null(m)) {
    ##if inverse matrix is not null return it
    message("getting cached data")
    return(m)
  }
  
  ##if inverse matrix is null; get original non-inversed matrix from object x
  data <- x$get()
  
  ## solve inverse matrix
  m <- solve(data, ...)
  
  ## set inversed matrix to object x variable inversedMatrix 
  x$setinverse(m)
  
  ##return inversed matrix
  m
}
