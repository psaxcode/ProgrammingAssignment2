# makeCacheMatrix: Given a matrix, this function creates a special "matrix" 
# object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cacheSolve should retrieve the 
# inverse from the cache.


## makeCacheMatrix takse a matrix as its argument and returns a special "matrix",
# which is really a list of following functions: 
# set(y): set a new matrix y;
# get(): get the matrix;
# setinv(inverse): set the inverse of the matrix;
# getinv(): get the inverse of the matrix;


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <-function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}



# The cacheSolve function computes the inverse of the special "matrix" created 
# with the above makeCacheMatrix function. It first checks to see if the inverse 
# of the matrix has already been cached. If so, it gets the inverse from the 
# cache and skips the computation. 
# Otherwise, it calculates the inverse of the given matrix and sets the inverse 
# matrix in the cache.
# It returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    inverse <- solve(x$get(),...)
    x$setinv(inverse)
    inverse
}
