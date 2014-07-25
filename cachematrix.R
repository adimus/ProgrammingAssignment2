
#These functions create a matrix and a variable to contain the results of computing the inverse of the matrix. The catched inverse of the matrix is outputed if the input matrix is not changed. If the input gets changed however, its inverse is computed and attributed to the special variable we created for it.


## MakeCacheMatrix() sets the input matrix, it creates the variable I to catch the inverse of the matrix. It defines the functions get(),  setInverse(), and getInverse() and keeps their values in a list. get() retrieves the input matrix, setInverse() computes the inverse of input matrix and catches the result in I. And getInverse() retrieves the catched inverse values of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) I <<- solve(x)
    getInverse <- function() I
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## cacheSolve() retrieves the catched inverse values (kept in variable I). If this matrix is not empty it outputs the Inverse matrix. If it is empty (the matrix's nverse hasn't been computed yet) it computes the inverse matrix and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getInverse()
        if(!is.null(I)) {
            message("getting cached data")
            return(I)
        }
        data <- x$get()
        I <- solve(data)
        x$setInverse(I)
        I
}
