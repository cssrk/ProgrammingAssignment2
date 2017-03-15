## The below functions demonstrate the cache mechanism
## for calculating the inverse of a matrix
## only when the matrix data is changed

## The makeCacheMatrix function provides a list of 4 functions
## set, get, setInverse and getInverse, which provide for
## getting a matrix data and providing the inverse of it
makeCacheMatrix <- function(x = matrix(nrow = 2, ncol = 2)) {
        s <- NULL
        # Check if the passed value is of type matrix
        # We can keep adding conditions here to check if it's a
        # square matrix and invertible matrix etc. and whether
        # it can be coerced into such a matrix etc. But for simplicity
        # checking for a strict matrix class (though this will indeed fail
        # even if a dataframe is passed which can be coerced)
        if (class(x) != "matrix") {
                print("Please pass a parameter of type matrix")
                return(tmp )
        }
        set <- function(y) {
                # Check if the passed value is of type matrix
                # We can keep adding conditions here to check if it's a
                # square matrix and invertible matrix etc. and whether
                # it can be coerced into such a matrix etc.
                if (class(y) != "matrix")
                        print("Please pass a parameter of type matrix")
                else {
                        # Reset the value only if it is different than existing value
                        if (!identical(x, y)) {
                                x <<- y
                                s <<- NULL
                        }
                }
        }
        get <- function() x
        setInverse <- function(solve) s <<- solve
        getInverse <- function() s
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function simulates the cache for the inverse of the matrix
## This calculates the inverse only if it is not already calculated
## If the data is same, then it simply returns the pre-saved inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setInverse(s)
        s
}
