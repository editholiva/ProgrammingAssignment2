## These pair of functions cache the inverse of a matrix

## First one: cache the inverse of a matrix creating a matrix object
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ##Setting the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }  
  ##Getting and returning the matrix
  get <- function() x
  
  ##Setting the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ##Getting and returning the inverse of the matrix
  getinverse <- function() inv
  
  ##List of the methods used in the main function
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
}


## Second one: calculate the inverse of a matrix returned by the makeCacheMatrix function.
## An important fact is that if the inverse has already beed calculated this function will retrive the inverse from the cache
cacheSolve <- function(x, ...) {
  ##Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ##Returning the inverse if its already set (from cache)
  if(!is.null(inv)) {
        message("getting cached data")
    return(inv)
  }
  
  ##Getting the matrix
  matrx <- x$get()
  
  ##Calculating the inverse of the matrix
  inv <- solve(matrx,...)
  
  ##Setting and returning the inverse of the matrix
  x$setinverse(inv)
  inv
        
}
