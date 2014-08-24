## Create two functions in order to caching inverse of a matrix rather than 
##compute it repeatedly. 

## creates a special "matrix" object which can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                inv <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- inverse
        getinverse <- function() inv
        list (set=set, get=get, setinverse=setinverse, 
              getinverse=getinverse)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cacheSolve should retrieve the inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)){
                message ("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv ## Return a matrix that is the inverse of 'x'
}
