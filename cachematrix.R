## Assignment: Caching the Inverse of a Matrix
## The following pair of functions look to cache the inverse of a matrix rather 
## than having to compute it repeatedly.

## 1. makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inver <- NULL
     set <- function(y) {
        x <<- y
        inver <<- NULL
     }
     get <- function() x 
     setInverse <- function(inverse) inver <<- inverse
     getInverse <- function() inver
     list(set = set, get = get,setInverse = setInverse,
        getInverse = getInverse)
}


## 2. cacheSolve: This function computes the inverse of the special "matrix" 
## returned by 'makeCacheMatrix' above. If the inverse has already been calculated
## (and the matrix has not changed), then 'cacheSolve' should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    inver <- x$getInverse() ## Return a matrix that is the inverse of 'x', assigned to inver
    if(!is.null(inver)) {   ## Set up condition for if inverse is not missing, then get cached data  
        message("getting cached data")
        return(inver) ## Inverse returned here
    }
    matdata <- x$get() ## matrix named matdata
    inver <- solve(matdata, ...) ## returns inverse of matrix
    x$setInverse(inver)
    inver
}


