## a pair of functions that cache the inverse of a matrix.

## this function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
         invm <- NULL
  ## this sets the value of the matrix
  set <- function(y) {
         x <<- matrix
         invm <<- NULL
  }
  ## this gets the value of the matrix
  get <- function() {
          x
  }
  ## this sets the inverse of matrix
  setinverse <- function(inverse){
          invm<<- inverse
  }
  ## this gets the inverse of matrix
  getinverse<-function(){
          invm
  }
  ## this returns the list of the methods used
  list(
          set=set,get=get,
          setinverse=setinverse,getinverse=getinverse
          )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated， then cacheSolve should retrieve the inverse from the cache.
## If the inverse is not available, cacheSolve should calculate it and save the answer in cache.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   invm <- x$getinverse()
   if(!is.mull(invm)){
           return(invm)
   }
   invm <- solve(x$get())
   x$setinverse(invm)
   return(invm)
}
