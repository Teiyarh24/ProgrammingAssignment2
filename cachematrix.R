## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        # Set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Get the matrix
        get <- function() x
        
        # Set the inverse
        setinverse <- function(inverse) inv <<- inverse
        
        # Get the inverse
        getinverse <- function() inv
        
        # Return a list of functions
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix hasn't changed), 
## it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Try to get cached inverse
        inv <- x$getinverse()
        
        # If inverse exists, return it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Otherwise, calculate the inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
