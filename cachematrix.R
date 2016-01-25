
##This function creates a special "matrix" object
##that can cache its inverse
##The makeCacheMatrix function returns a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        ## calculating the inverse of the matrix using solve() function
        setinverse <- function(solve) m <<- solve
        
        ## getting the inverse of the matrix
        getinverse <- function() m
        
        ##returning the list of functions: set, get, setinverse, getinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheInverse <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()

        ##get the original matrix
        mat <- x$get()

        ##check if the original matrix has not changed
        identicalmatrices <- isIdentical(mat, x)

        if(!is.null(m)) {
                message("matrix did not change; getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## Funcion to determine if 2 matrices are exactly identical (both values and dimensions should match)
isIdentical <- function(matrix1, matrix2) {
	##is.matrix(matrix1) && is.matrix(matrix2) && dim(matrix1) == dim(matrix2) && all(matrix1 == matrix2)
}
