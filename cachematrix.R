## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a list of four functions set which sets
## the matrix, get which gets the matrix, setinverse which sets the inverse
## of the matrix, and getinverse which gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cachSolve function either retrieves the cached value of the inverse of the
## matrix or it calculates it.  It does this by calling the functions created
## in the MakeCacheMatrix
## note - the assignment did not include requirements about error handling for
## non a invertible matrix so that has not been handled.  Passing this type
## of matrix will result in an error

cacheSolve <- function(x, ...) {

	  m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m



        ## Return a matrix that is the inverse of 'x'
}
