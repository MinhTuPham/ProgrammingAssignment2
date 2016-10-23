## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

## makeCacheMatrix include a list of functions: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. calculate the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## 1. Check if the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
## 2. Otherwise, it calulates the inverse of the matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
