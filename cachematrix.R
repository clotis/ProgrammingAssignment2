
## The idea here is to create a special 'matrix' able to contain its inverse
## The goal is to calculate its inverse only the first time we need it, and then cache it

## This function create a special 'matrix', which contains 4 function
## set: set the matrix
## get: get the matrix
## setinverse: set the inverse of the matrix
## getinverse: get the inverse of the matrix from cache

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL #this variable contains the inverse of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of a matrix created using 
## the makeCacheMatrix function
## If the cached value of the inverse is NULL then this function calculates it 
## using the solve() function, else it returns the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        
        # Use he solve() function to calculate the inverse of the matrix        
        i<-solve(data)
        x$setinverse(i)
        i
}
