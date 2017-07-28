## Put comments here that give an overall description of what your
## functions do

## @x : a matrix which is invertible
## get : return the matrix
## set : change the matrix => so the inverse Matrix is set to Null
## setInverse : set the calculed Inverse
## getInverse : get the Inverse ( if the Inversed Matrix is not already calculated then null is returned)
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## @x : a makeCacheMatrix
## this function first checks if there already is a calculated Inversed matrix
## if yes then it return the inversed matrix
## else it computes the inversed matrix and set the result in the cache
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m))
    {
      message("getting cached data")
      return(m)
    }
    myMatrix <- x$get()
    inverse <- solve(myMatrix,...)
    x$setInverse(inverse)
    inverse
}
