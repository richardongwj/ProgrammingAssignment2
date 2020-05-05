## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ##set the value of matrix to NULL
        m <- NULL
        ##set matrix - x to new matrix, y, and reset the inverse, m, to NULL.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ##get the value of matrix, x
        get <- function() x
        ##set the value of inverse
        setinverse <- function(inverse) m <<- inverse
        ##get the value of inverse
        getinverse <- function () m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Functions calculates the inverse of the matrixed from makeCacheMatrix function. 
## It compares with previous calculation before attempting calculation. 
## If it was previously calculated, it will retrieve previous results "m".

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() ##compares with previous calcultion results stored as m.
        if(!is.null(m)) {
                message("getting cached data")
                return(m) ##results of previously calculated m
        }
        data <- x$get()
        m <- solve(data, ...) ##new result into m.
        x$setinverse(m)
        m 
}
