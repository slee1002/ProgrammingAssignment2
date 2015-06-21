## makeCacheMatrix creates a special "matrix" which is really a list
## containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setminv <- function(minverse) minv <<- minverse
        getminv <- function() minv
        list(set = set, get = get,
             setminv = setminv,
             getminv = getminv)
}


## returns the matrix inverse from cache if previously cached or by
## using solve() if not previously cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getminv()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setminv(minv)
        minv
}
