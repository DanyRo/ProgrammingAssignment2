##Daniela RR  18.08.2015

##Matrix inversion is a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly, therefore this pair of funtions set a Matrix, solve the inverse and retreive the inverse 
## from the cache if the matrix didn't change and the inverse has already been calculated.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                
                #Initializiting the matrix.
                invmatrix <- NULL
                
                # Assign a value to x and define it out of it's own environment
                set <- function(y) {
                        x <<- y
                        invmatrix <<- NULL
                }
                
                # gets the matrix that was passed to the function makeCacheMatrix
                get <- function() {
                        x
                }
                
                #sets the solution of the inverted matrix
                setinvmatrix <- function(invertedx) {
                        invmatrix <<- invertedx
                }
                
                #if not computed yet gets the solution for the inverted matrix
                getinvmatrix <- function() {
                        invmatrix
                }
                
                #makes a list with all the funtions previously defined
                list(set = set, get = get,
                     setinvmatrix = setinvmatrix,
                     getinvmatrix = getinvmatrix)
        
}


## cacheSolve: finds the inverse of a matrix created by makeCacheMatrix, if the 
## inverse has been computed previously it retreives the solution from cache and 
## prints a message "getting cached data"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
                invmatrix <- x$getinvmatrix()
                if(!is.null(invmatrix)) {
                        message("getting cached data")
                        return(invmatrix)
                }
                data <- x$get()
                invmatrix <- solve(data, ...)
                x$setinvmatrix(invmatrix)
                invmatrix
       
}
