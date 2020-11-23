## This pair of functions cache the inverse of a square invertable matrix. 
## When the inverse of the unchanged latter is needed again, this pair of functions 
## saves computational costs by looking up the inverse from cache rather than 
## recomputing it again.

## This function creates a list that contains functions that set and get each of 
## the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<- function(y){
    x<<- y
    inv <<- NULL
  }
  get<- function() x
  setInverse <- function(inverse) inv <<-inverse
  getInverse <- function() inv
  list(set=set, get=get,
       setInverse=setInverse, getInverse=getInverse)
}


## This function uses the functions in the list created by the first function to 
## retrieve the inverse from cache if it has already been calculated, or calculate
## and cache the value otherwise.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv))
    return(inv)
  data<- x$get()
  inv<- solve(data, ...)
  x$setInverse(inv)
  inv
}
