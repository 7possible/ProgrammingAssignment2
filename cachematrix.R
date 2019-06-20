## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
        

## This function set the cached Matrix "cacheMatrix" to the inverse of the matrix x provided as parameter

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' by using cached version if already computed
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Inverse aleready exists!")
                message("getting cached data")
                return(inv)
        }
        originalMatrix <- x$get()
        message("Calculating inverse of matrix")
        inv <- solve(originalMatrix)
        x$setinverse(inv)
        inv 
}


### ==Results====

##> source("cachematrix.R")
##> cachem <- makeCacheMatrix(cbind(c(-10,5),c(5,10)))   ## New matrix values
##> cacheSolve(cachem)             ## first time new maatrix value
##Calculating inverse of matrix
##[,1] [,2]
##[1,] -0.08 0.04
##[2,]  0.04 0.08
##> cacheSolve(cachem)            ## Second time same maatrix 
##Inverse aleready exists!
##        getting cached data
##[,1] [,2]
##[1,] -0.08 0.04
##[2,]  0.04 0.08
##> cacheSolve(cachem)            ## Third time same maatrix 
##Inverse aleready exists!
##        getting cached data
##[,1] [,2]
##[1,] -0.08 0.043
##[2,]  0.04 0.08
##> cachem <- makeCacheMatrix(cbind(c(-2,.5),c(.5,10)))      ## New matrix values
##> cacheSolve(cachem)       ## first time new maatrix value
##Calculating inverse of matrix
##[,1]       [,2]
##[1,] -0.49382716 0.02469136
##[2,]  0.02469136 0.09876543
##> cacheSolve(cachem)       ## Second time same matrix
##Inverse aleready exists!
##        getting cached data
##[,1]       [,2]
##[1,] -0.49382716 0.02469136
##[2,]  0.02469136 0.09876543
##> cacheSolve(cachem)      ## third time same matrix
##Inverse aleready exists!
##        getting cached data
##[,1]       [,2]
##[1,] -0.49382716 0.02469136
##[2,]  0.02469136 0.09876543