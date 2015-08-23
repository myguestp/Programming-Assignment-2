## Note:
## I am Spanish and it is posible that i have any mistake 
## when i write the descriptions of these functions.

## These two functions allow us to create a special "matrix" object
## that store a matrix and calculate just once his inverse.
## The inverse of our matrix is stored in cache and dont need to be
## recalculated when we need it another time

## This function create a special "matrix" object where we can 
## store a matrix and his inverse.
## Really, this function returns a list of functions that allow us
## to set a matrix, get the given matrix, calculate his inverse and get it

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
        
}


## This function verify a special "matrix" object created before and verifies if
## his inverse has been calculated. If his inverse has already been calculated
## the function returns the inverse matrix stored in cache and otherwise, the
## fuction calculate the inverse, stores it in cache and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
