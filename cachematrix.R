## Caching the inverse of a matrix rather than repeated computation for better optimisation of
## a costly process.

# The functions makeCacheMatrix and cacheSolve cache the inverse of a matrix

## makeCacheMatrix creates a list that contains a function for performing the following tasks:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialising the inverse to NULL
    inverse <- NULL
    
    ## Set the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ## Get the matrix
    get <- function() x
    
    ## Set the inverse of the matrix
    setInv <- function(inv) inverse <<- inv
    
    ## Get the inverse of the matrix
    getInv <- function() inverse
    
    ## Return a list of methods
    list(set = set,
        get = get,
        setInv = setInv,
        getInv = getInv)

}

## cacheSolve returns the inverse of the matrix.
cacheSolve <- function(x, ...) {
    
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInv()
        
        ## Return the inverse if it already exists.
        if(!is.null(inverse)) {
            message("getting cached data.")
            return(inverse)
        }
        
        ## Get the matrix
        data <- x$get()
        
        ## Solve the matrix to get the inverse and assign it to inverse
        inverse <- solve(data)
        
        ## Set the inverse of the matrix
        x$setInv(inverse)
        
        ## Return the inverse
        inverse
}
