## Functions allow you to cache a matrix and its inverse

## Function sets and returns a matrix passed as argument x or by calling
## the .set() function; also returns the inverse matrix if computed in
## cacheSolve() function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # set matrix method, if not passed as arg set to NULL
        set <- function(y) {
               x <<- y
               m <<- NULL
        }

        # return matrix by calling get
        get <- function() x

        # return inverse which is set in cacheSolve
        getInverse <- function() m

        # allows setting of inverse matrix via cacheSolve
        setInverse <- function(inverse) m <<- inverse

        list(set = set, get = get, getInverse = getInverse,
             setInverse = setInverse)

}



## Function either computes inverse of matrix passed as x or
## reads cached version of inverse of matrix stored in m

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        ## retrieve inverse of x
        m <- x$getInverse()

        ## test to see if inverse has already been calculated
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        ## if inverse not cached then use solve() to compute inverse
        data<-x$get()
        m <- solve(data)

        ## call setInverse method to set inverse
        x$setInverse(m)

        # return inverse matrix
        m
}


