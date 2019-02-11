## USAGE example -----------------------------------------------------------

# m <- makeCacheMatrix(matrix(rnorm(25),5,5)) # generate the special matrix object
# m$get() # print out the matrix object values ()
# m_inv <- cacheSolve(m)
# m_inv_test <- solve(m$get()) # the same inverse should this be
# identical(m_inv, m_inv_test) # this I expected would still be false due to
# rounding errors in floating points but it seems to work:)

## ------------------------------------------------------------------------

## function creates a "matrix class" that contains 4 functions to manipulate
## the matrix object with (set value, get value, set inverse, get inverse)
makeCacheMatrix <- function(m = matrix()) {

     inv <- NULL
     set <- function(new_m) {
         m <<- new_m
         inv <<- NULL
     }
     get <- function() m
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## function that calculates the inverse of the special "matrix class" created
## by the makeCacheMatrix() function; it first checks if the inverse has already
## been calculated and if yes, it loads the inverse matrix from cache. If not,
## it calculates the inverse by the solve() function and sets it to the matrix object
cacheSolve <- function(m, ...) {

    inv <- m$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    m_data <- m$get()
    inv <- solve(m_data, ...)
    m$setinverse(inv)
    inv # return inverse
}