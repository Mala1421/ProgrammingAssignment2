## This R code contains a pair of functions that is able to cache the Inverse of a Matrix,
## which a potentially time consuming or costly computation.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- matrix()
        set <- function(y) {
                x <<- y
                i <<- matrix()
        }
        get <- function() x #stores the original matrix data in get
        
# Here, solve, needs to be in lower case, R is case sensitive to calling functions including variables.
        setSolve <- function(solve) i <<- solve    #caches the inverse to parent environment.           
        
        getSolve <- function() i
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getSolve()
        if(!(identical(i,(matrix())))) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setSolve(i)
        i
}


## Remove comment tag, #, from below (mt,mc,cacheSolve(mc)) only, to test the above functions

#mt <- matrix(data = c(4,7,2,6), nrow = 2, ncol=2, byrow = TRUE)          # A square invertible matrix 

#mc <- makeCacheMatrix(mt)     # converts matrix to cacheable list data

#cacheSolve(mc) #At first, solves for the inverse of matrix, mt. Subsequently, it gets inverse from cache




# Matrix inversion is usually a costly computation and there may be 
# some benefit to caching the inverse of a matrix rather than compute 
# it repeatedly (there are also alternatives to matrix inversion that 
# we will not discuss here). Your assignment is to write a pair of 
# functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	cachedInverse <- NULL
	getSolved <- function() cachedInverse
	getMatrix <- function() x
	setSolved <- function(im) cachedInverse <<- im
	setMatrix <- function(newMatrix) {
		x <<- newMatrix
		cachedInverse <<- NULL
	}
	list(getMatrix = getMatrix, getSolved = getSolved, 
		 setSolved = setSolved, setMatrix = setMatrix)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inverseMatrix <- x$getSolved()
	if (!is.null(inverseMatrix)) {
		## The inverse has already been cached
		message("Using cached inverse.")
		return(inverseMatrix)
	}
        ## Solve for a matrix that is the inverse of 'x', cache it, and return it
	theMatrix <- x$getMatrix()
	inverseMatrix <- solve(theMatrix, ...)
	x$setSolved(inverseMatrix)
	return(inverseMatrix)
}

## Function to test cacheSolve()
## Adapted from http://stackoverflow.com/questions/19106015/r-how-to-generate-random-yet-easily-invertible-matrices
generateRandomMatrix <- function(aSeed = 123,size = 3000) {
	set.seed(aSeed)
	return(matrix(runif(size^2),size))
}


## My solution
## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
