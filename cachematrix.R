#### ProgrammingAssignment2 solution  by Anita Datta Chowdhury submitted on 03/24/2017

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.

# function 1: makeCacheMatrix. This function creates a special matrix that can cache its inverse. 
# It takes a matrix as input and list an output

# starting makeCacheMatrix
makeCacheMatrix <- function(m = matrix()) {
        
        # initialize the invert matrix to null
        im <- NULL
        
        # set matrix
        set <- function(y) {
                m <<- y
                im <<- NULL
        }
        
        # get matrix
        get <- function(){
                m
        }
                
        # setting inverse value 
        setinverse <- function(inverse) {
                im <<- inverse
        }
        
        # Getting inverse value
        getinverse <- function() {
                im
        }
        
        # output the list with getter and setter functions 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
# ending makeCacheMatrix

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

# starting cacheSolve function
cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Check if inverse value exist, if found return the cached data
        im <- m$getinverse()
        
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }

        # get data        
        data <- m$get()
        
        # calculate inverse value
        im <- solve(data, ...)
        
        # return inverse value
        m$setinverse(im)
        im
}
# end of CacheSolve function