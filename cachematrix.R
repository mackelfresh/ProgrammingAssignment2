## These functions take a matrix, store it, calculate the inverse 
## of the matrix, and cache it so as to not require heavy repeated
## computation

## This first function creates an object which is really a list 
## that contains a function to set and get the values of a matrix 
## and also sets and gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
             x <<- y
             inv <<- NULL
             
     }
     get <- function() x
     setInverse <- function(inverse) inv <<- inverse
     getInverse <- function() inv
     list(set = set, get =get, 
          setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the inverse of the results from the 
## first function above.  If it's been calculated already, it will 
## get the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        m <- x$get()
        inv <- solve(m, ...)
        x$setInverse(inv)
        inv
}
