## This function creates a special "matrix" object with three subfunctions
## $setcache - caches the matrix passed to it and solves and caches its inverse
## $getcache - retrieves the cached matrix
## $getinverse - retrieves the cached inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## caches the matrix passed to it and solves and caches its inverse
        setcache <- function(x) {
                message("Setting the new cache")
                cachematrix <<- x  ## caches matrix
                message("Inverting the matrix")
                cacheinverse <<- solve(cachematrix)  ## solves and caches the inverse
        }
        
        ## retrieves the cached matrix for comparison purposes
        getcache <- function() {
                message("Geting the cached matrix")
                cachematrix # return the cached matrix
        }
        
        ## retrieves the cached inverse       
        getinverse <- function() {
                cacheinverse # return the inversed matrix
        }
        
        # return a list containing the subfunctions
        list(setcache = setcache, 
             getcache = getcache,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the matrix has not changed and the inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x,z=matrix()) {
        cache <- x$getcache() # get cached matrix
        
        message("matrix input")
        print(z)
        message("cached matrix")
        print(cache)
        
        if(identical(cache,z)) {  ## check to see if it's identical to the new matrix
                message("Matrix is IDENTICAL to cached matrix")
        }
        ## if the matrix is different, update cache with new matrix and its inverse
        else {                    
                message("Matrix is DIFFERENT than cached matrix")
                x$setcache(z)
        }
        message("Return the inverse")
        matr <- x$getinverse() ## returns the inverse matrix
}

## Test
## Initialize cached matrix and inverse
cachematrix <<- 0
cacheinverse <<- 0
## generate a random square, non-singular matrix
test1 <- matrix(runif(9,1,100),3,3)
test2 <- matrix(runif(9,1,100),3,3)
test3 <- matrix(runif(16,1,100),4,4)
test4 <- matrix(runif(16,1,100),4,4)

## Test1 - cacheSolve with a 3x3 matrix; matrix multiply input with inverse for identity matrix
test <- cacheSolve(makeCacheMatrix(),test1)
## Test returned inverse matrix, matrix multiply with input to produce identity matrix
print(round((test %*% test1),0))

## Repeat test1 3x3 matrix
test <- cacheSolve(makeCacheMatrix(),test1)
## Test returned inverse matrix, matrix multiply with input to produce identity matrix
print(round((test %*% test1),0))

## Test2 - 3x3 matrix
test <- cacheSolve(makeCacheMatrix(),test2)
## Test returned inverse matrix, matrix multiply with input to produce identity matrix
print(round((test %*% test2),0))

## Test3 - 4x4 matrix
test <- cacheSolve(makeCacheMatrix(),test3)
## Test returned inverse matrix, matrix multiply with input to produce identity matrix
print(round((test %*% test3),0))

## Test4 - 4x4 matrix
test <- cacheSolve(makeCacheMatrix(),test4)
## Test returned inverse matrix, matrix multiply with input to produce identity matrix
print(round((test %*% test4),0))

## Repeat Test4 4x4 matrix
test <- cacheSolve(makeCacheMatrix(),test4)
## Test returned inverse matrix, matrix multiply with input to produce identity matrix
print(round((test %*% test4),0))
