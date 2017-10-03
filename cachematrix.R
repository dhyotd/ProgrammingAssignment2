
##The first function, 'makeCacheMatrix', creates a special 'matrix' comprised
## of a list of functions that:
## 1) set the value of a matrix, 'set'
## 2) get the value of that matrix, 'get'
## 3) set the value of the inverse, 'setinverse'
## 4) get the value of the inverse, 'getinverse'

makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invs <<- inverse
        getinverse <- function() invs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## The second function, 'cacheSolve', checks to see if the inverse of a matrix
## has already been calculated. If it has, it returns the inverse that has been
## stored in the cache by the previous function. If not, it calculates the
## inverse of the matrix and stores it in the cache with 'setinverse'.

cacheSolve <- function(x, ...) {
                invs <- x$getinverse()
                if(!is.null(invs)) {
                        message("getting cached data")
                        return(invs)
                }
                mat <- x$get()
                invs <- solve(mat, ...)
                x$setinverse(invs)
                return(invs)
        }

