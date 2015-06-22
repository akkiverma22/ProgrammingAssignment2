## function to get inverse of an inversible matrix

## function to create a list of matrix , get and set method 
        
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse property
        m <- NULL
        
        ## Method to set the matrix
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        ## Method the get the matrix
        get <- function() x
        
        ## Method to set the inverse of the matrix
        setmatrix <- function(solve) m <<- solve
        
        ## Method to get the inverse of the matrix
        getmatrix <- function() m
        
        ## Return a list of the methods
        list( set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## calculate the inverse of the given matrix 
## if matrix not changed get inverse from cache

cacheSolve <- function( x = matrix(), ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        
        ## Just return the inverse if its already set
        if( !is.null(m) ){
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix from our object
        matrix <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        m <- solve(matrix, ...)
        
        ## Set the inverse to the object
        x$setmatrix(m)
        
        ## Return the matrix
        m
}
