## Matrix inversion is usually a costly computation
## The below functions provide a way to cache the inverse of a matrix 
## and retrieve it when needed instead of calculating it multiple times

## Function makeCacheMatrix creates a special "matrix" object that stores the matrix itself, its inverse
## and four methods/functions for getting access to the matrix and its inverse

## The four methods are set, get, setinv and getinv

makeCacheMatrix <- function(x = matrix()) {    ## creates the matrix object that takes in a matrix 'x'
        m <- NULL							   ## erases any cached matrix inverse that may exist in variable m
        set <- function(y) { 				   ## SET method/function saves a matrix to insert into the matrix object
                x <<- y						   ## copies the matrix into variable x in parent environment
                m <<- NULL					   ## erases old cached inverse from previous matrix (if it exists) in parent environment
        }
        get <- function() x					   ## GET method/function returns the matrix x stored in the matrix object
        setinv <- function(solve) m <<- solve  ## SETINV calculates and stores new inverse
        getinv <- function() m				   ## GETINV method returns stored inverse
        list(set = set, get = get,			   ## LIST shows the methods for getting access to the matrix
             setinv = setinv,				   ## and for getting access to the inverse
             getinv = getinv)
}


## Function cacheSolve takes a matrix object as a parameter and uses the object methods/functions
## to get access to the matrix itself and its stored inverse. The function returns the inverse of the matrix 
## if it exists, otherwise the function will calculate a new inverse using the matrix object and return the new inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()								## use GETINV method to get inverse from the matrix object
        if(!is.null(m)) {							## check if inverse already cached 
                message("getting cached data")		## if inverse is already cached, print message
                return(m)							## and return cached value
        }
        data <- x$get()								## otherwise get matrix from the matrix object
        m <- solve(data, ...)						## and calculate the inverse
        x$setinv(m)									## store the inverse in SETINV
        m											## return the inverse in the parent environment
}
