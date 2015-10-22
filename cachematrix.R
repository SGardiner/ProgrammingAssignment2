
## This function creates a special "matrix" object that can cache its inverse.
## It also caches the original matrix for comparison purposes

makeCacheMatrix <- function(x = matrix()) {
        cachematrix <<- x  # cache matrix x for comparison purposes
        cacheinverse <<- solve(x)  # cache the inverse of x
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the matrix has not changed and the inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(y, ...) {
        if (!identical(y,cachematrix)) {  ## Check to see if matrix is identical to last cached
                makeCacheMatrix(y)  ## If not identical, calculate the inverse
                message("Not identical and cached, calculating new inverse")
        }
        
        ## print(round((cachematrix %*% cacheinverse),0))
        return(cacheinverse)  ## Return a matrix that is the inverse of 'x'
        
}
