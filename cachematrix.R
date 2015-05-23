## Below are two functions that are used to create a special object that stores a numeric matrix and cache's its inverse.
## They take advantage of the lexical scoping rules of the R language and use the R <<- operator to manipulated and preserve state inside of an R object.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing functions to
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse of the matrix
##   get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with the above function makeCacheMatrix. However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
