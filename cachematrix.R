## Creates a special "cachematrix" object for a given matrix.
## The object is list of 4 functions as an interface to the
## internal matrix and the inverse(if already calculated).
makeCacheMatrix <- function(X = matrix()) {
    # internal variables
    X_ <- X
    inv_ <- NULL

    #   set <- function(X) {
    #     X_ <<- X
    #     inv_ <<- NULL
    #   }
    set <- function(X) X_ <<- X
    get <- function() X_

    setinv <- function(inv) inv_ <<- inv
    getinv <- function() inv_

    #return list of these 4 functions which "wrap" X and inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(X, ...) {
    ## Return a matrix that is the inverse of the matrix in object 'X'
    inv <- X$getinv()

    if(is.null(inv)) { #inverse not yet calculated
        mat <- X$get()
        inv <- solve(mat, ...)
        X$setinv(inv)
    } else {
        message("getting cached data")
    }

    inv
}
