## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# creates matrix-object with caching function
makeCacheMatrix <- function(x = matrix()) {
    data <- x                       # the matrix-data
    inv <- NULL                     # cache for the inverted matrix
    set <- function(y)              # sets matrix-data and resets cache
    {
        data <<- y
        inv <<- NULL
    }
    get <- function() data          # returns matrix-data
    setInv <- function(i) inv <<- i # sets the cache
    getInv <- function() inv        # returns cache
    list(set = set, get = get,      # list-construct to make functions available via $-notation
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function

# takes cachematrix-objext and returns its inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()        # call getInv function of cachematrix-object
    if(!is.null(inv)) {      # if inverse already cached...
        message("getting cached data")
        return(inv)          # ...return cached inverse
    }
    data <- x$get()          # ...else get data
    inv <- solve(data, ...)  # ...and calculate (solve) inverse
    x$setInv(inv)            # save inverse in cache of cachematrix-object
    inv                      # return inverse
}

