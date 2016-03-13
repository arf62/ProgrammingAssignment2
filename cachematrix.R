## x<-a square matrix
## z <- makeCacheMatrix(x) creates a matrix which will hold the inverse of x 
## cacheSolve(z) returns the inverse if X
## cacheSolve(z) this time it will return the inverse from the cache. 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                Inverse <- NULL
                setMatrix <- function(Matr)
                {
                 x<<-Matr
                 Inverse<<- NULL
                }
                getMatrix <- function() x
                setInverse<-function(Inv) Inverse <<-Inv
                getInverse<-function() Inverse
                
                list(setMatrix=setMatrix,getMatrix=getMatrix,
                     setInverse = setInverse,
                     getInverse = getInverse)
}


## Write a short comment describing this functions

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
                  Inverse <-x$getInverse()
                  if(!is.null(Inverse)){
                    message("getting cached Inverse")
                    return(Inverse)
                  }
                  matr <-x$getMatrix()
                  Inverse <- solve(matr,...)
                  x$setInverse(Inverse)
                  Inverse
}
