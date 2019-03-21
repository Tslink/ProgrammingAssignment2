
## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (mx){
    x <<- mx
    m <<- NULL
    
  }
  ## Store the value of the matrix and setter/getter of matrix 
  get <- function ()x  
  setinverse <- function(inverse) m <<- inverse 
  getinverse <- function () m 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 

}
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##             retrive the inverse from the cache.



## Return a matrix that is the inverse of 'x'
#check if m is not null , if m is not null return the cache matrix

cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()
  if(!is.null(m)) { 
    message("getting cached data") 
    return(m) 
  }
  data <- x$get() 
  m <- solve(data, ...)
  x$setinverse(m) 
  return(m)
}