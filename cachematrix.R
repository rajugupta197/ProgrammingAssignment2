## makeCacheMatrix: This function creates a special "matrix" object 
#  that can cache its inverse in the object 'inv'. Also 'MasterMatrix'
#  is a cached matrix to check if matrix has not changed.

makeCacheMatrix <- function(x = matrix()) {
    MasterMatrix <- NULL
    inv <- NULL
    set <- function(y) {
        x <<- y
        MasterMatrix <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(InvMat) {
        MasterMatrix <<- x
        inv <<- InvMat
    } 
    getinv <- function() {
        if(!is.null(MasterMatrix)) {
            if(is.matrix(x) && is.matrix(MasterMatrix) && dim(x) == dim(MasterMatrix) && all(x == MasterMatrix)) {
                inv
            }
        }
        else {
            return(NULL)
        }
    } 
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been
# calculated (and the matrix has not changed), then cacheSolve retrieves
# the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # checking if previous inverted cached matrix is available
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverted matrix")
        return(inv)
    }
    
    # if previous inverted cached matrix is NOT available 
    # then see if original matrix is not null and also check
    # if it is a square matrix then invert the matrix
    matx <- x$get()
    inv <- NULL
    if(is.matrix(matx) && dim(matx)[1] == dim(matx)[2]) {
        inv <- solve(matx, ...)
    }
    
    x$setinv(inv)   # calling function to chache invertion matrix
    inv
}
