## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inverseMatrix <- NULL #define inversed matrix here
        
        #set function to set orginal matrix
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        
        get <- function() x #get function to get original matrix
        
        setInversed <- function(inverse) inverseMatrix <<- inverse #used to set inversed matrix
        
        getInversed <- function() inverseMatrix #used to get inversed matrix
        
        #return a list of functions to communicate with the cache
        list(set=set, get=get,
             setInversed =setInversed,
             getInversed=getInversed )
}
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversed <- x$getInversed()
        
        # if inversed matrix is available, return it.
        if(!is.null(inversed)){
                message("getting cached data")
                return(inversed)
        }
        
        # if inversed matrix is not available, calculate it.
        originalMatrix <- x$get()
        inversed <- solve(originalMatrix)
        x$setInversed(inversed)
        inversed
}
