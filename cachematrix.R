## Creates a special "cachematrix" object for the matrix given.
## The object is list of 4 functions as an interface to the
## internal matrix and its inverse(if already calculated).
makeCacheMatrix <- function(X = matrix()) {
    # internal variables
    X_ <- X
    inv_ <- NULL

    set <- function(X) X_ <<- X
    get <- function() X_

    setinv <- function(inv) inv_ <<- inv
    getinv <- function() inv_

    #return list of these 4 functions which "wrap" X and inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Returns a matrix that is the inverse of the cachematrix object 'X'
## If it's not cached yet, we calculate it first.
cacheSolve <- function(X, ...) {
    inv <- X$getinv()

    if(is.null(inv)) {
        # inverse not yet cached, calculate and set it
        mat <- X$get()
        inv <- solve(mat, ...)
        X$setinv(inv)
    } else { 
        # inverse already cached
        message("getting cached data")
    }

    inv
}
