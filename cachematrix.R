## Michael Hardisty
## michaelmrh@gmail.com

## Creates a Cachematrix object

makeCacheMatrix <- function(x = matrix()) {
     ## CacheMatrix objects have a special variable m that is the cashed inverse
     m <- NULL
     ## set changes what matrix the Cachematrix represents
     set <- function(y) {
          x <<- y
          ##We set m to NULL because the new matrix may have a different inverse
          m <<- NULL 
     }
     ##get returns the matrix x the cacheMatrix represents
     get <- function() x
     #setinverse stores the solve to the value m
     setinverse <- function(solve) m <<- solve
     #get inverse retrieves m
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## CacheSolve checks if the inverse has already been computed and stored in m
## If it has, it prints "getting cached data" and returns the cached value
## If it hasn't, it computes the inverse and stores it in m
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          ##the return statement stops the function here so it does not
          ##recalculate the inverse
          return(m)
     }
     ##data is the matrix the cacheMatrix represents
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
