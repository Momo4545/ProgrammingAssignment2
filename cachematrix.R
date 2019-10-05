## First function is 'makeCacheMatrix' that will generate a list of functions to be used in the
## next function 'cacheSolve' that will return a cached value of if the inverse is already computed
## and if not it will compute the inverse




## This function generates a list of functions that can set(change) and get (return) values
## of the objects x and i where x is initialized as a empty matrix and i is initialized as NULL
## but is later intended to hold the inverse of x. The function to set x also makes i as NULL as
## it will have to be recalculated

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function first checks if the inverse i is empty. If it is not emtpy(function has run before
## without a change in x) then it returns the same value for i. If it is null it computes the inverse
## of x and returns that

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
        if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
