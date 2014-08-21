## Determines the inverse of a mitrix using the solve() function; caches result
## Firstly checks if cached result is available; if so, uses that and no need to re-calculate

## Creates an object of class list
## Stores the inverse of the matrix calculated in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()  {
                x
        }
        setinv <- function(inv) {
                m <<- inv
        }
        getinv <- function() {
                m
        }
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Determines if inverse of matrix has already been determined
## - if not, this is calculated then sent to makeCacheMatrix
## - if so it retrieves the cached matrix inverse
## The inverse of the matrix is then returned

cacheSolve <- function(w, ...) {
        ## Return a matrix that is the inverse of 'w'
        m <- w$getinv()
        if(!is.null(m)) {
                message("getting cached data...")
                return(m)
        }
        data <- w$get()
        m <- solve(data, ...)
        w$setinv(m)
        m
}
