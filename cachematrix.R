## The below two functions makeCacheMatrix and cacheSolve are written to implement the caching of inverse of a matrix

## This function makeCacheMatrix sets and gets the input matix passed and the inverse of the matrix calculated 

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function cacheSolve checks if the inverse of the matrix is cached already. If yes, then it returns the cahced matrix inverse
## If not it calculates and returns the matrix inverse
## This function also calls the setinverse function in the MakecahcheMatrix function to cache the caclculated matrix inverse


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}