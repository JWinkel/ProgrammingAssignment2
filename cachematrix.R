## This set of functions allows for the input of a matrix and solving for the
## the inverse of that matrix. If the matrix has already been solved, the 
## functions will retrieve that solved value from the cache, saving valuable
## computing time.


## The makeCacheMatrix function which creates a matrix, but more importantly is 
## really a list of 4 fuctions to "get" and "set" the values of the matrix and 
## the values of the inverse. These 4 functions are how the function can cache
## its inverse. The set function changes the matrix stored in the main function 
## via the <<- operator. The get function "gets" the matrix x stored in the main 
## function, e.g. a$get() returns the matrix you've inputted initially (assuming 
## your matrix is named a). The setinverse stores the value of input into 
## variable m (doesn't actually perform the solve calculation). The getinverse 
## function returns the variable m set in setinverse. The final list stores the 
## individual functions in a list so that when we assign makeCacheMean to an 
## object, the object has all 4 functions. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { 
                x <<- y
                m <<- NULL
        }
        get <- function() x 
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the matrix set in 
## makeCacheMatrix. If the inverse has already been calculated then the 
## cacheSolve function should retrieve the inverse from the cache. It then 
## returns a matrix that is the inverse of 'x'. The function initially checks to
## see if a value has already been assinged to m via the !is.null condition in 
## the if condition.

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
}
