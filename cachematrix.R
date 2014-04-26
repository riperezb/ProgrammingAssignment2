## Below are two functions that are used to create 
## a special object that stores a matrix and cache's its inverse

## makeCacheMatrix creates a list object containing a function to:
##  1. Set the value of the matrix x
##  2. Get the value of the matrix x
##  3. Set the value of the inverse of the matrix x
##  4. Get the value of the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    ##Here the list is created. Each element is a function to be called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix obtained from the special vector created by 
## makeCacheMatrix. Note that:
##  1. cacheSolve first checks if the inverse has been calculated,
##  2. if so, it retrieves the inverse matrix from the getinverse function and skips computation.
##  3. Otherwise, it gets the original matrix and calculates the inverse matrix.
##  4. Then it updates the object created by makeCacheMatrix (using setinverse), and
##  5. it finally retrieves the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
