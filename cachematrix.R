## Solution to Programming Assigment 2
## creates 2 functions - 
## one for creating a special type of object that can cache the matrix inverse; 
## another for checking if the inverse is already in the cache and returing it if so. Otherwise it calculates the inverse and caches it

## Usage demonstration:
## create a matrix that we will use for testing:
## m1 <- matrix(c(4,5,6,3,4,5,11,44,66), 3,3)

## create a special object that holds the matrix and its inverse. There is no inverse yet:
## ca1 <- makeCacheMatrix(m1)
## get the matrix inverse. There is no pre-cached inverse, so it has to be calculated:
## u <- cacheSolve(ca1)
## print the inverse:
## u
##             [,1]        [,2]        [,3]
## [1,] -4.00000000  13.0000000 -8.00000000
## [2,]  6.00000000 -18.0000000 11.00000000
## [3,] -0.09090909   0.1818182 -0.09090909
##
## get the matrix inverse again. This time is it retrieved from cache and a message is displayed:
## w <- cacheSolve(ca1)
## we have cached matrix, returning
##
## print the inverse to verify that it is the same
## w

## Creates an object that can hold a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    getMatrix <- function() x
    setMatrix <- function(y) {
      x <<- y
      i <<- NULL
    }
    getInverse <- function() i
    setInverse <- function(inverse) {
      i <<- inverse
    } 
    list(getMatrix = getMatrix, setMatrix = setMatrix, getInverse = getInverse, setInverse = setInverse)
}


## Accepts special object that holds a matrix and its inverse 
## returns an inverse from the cache if there is one. Otherwise calculates the inverse, caches it returns it. 
cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if (!is.null(i)) {
          message("we have cached matrix, returning it")
          return(i)
        }
        ## there is no inverse calculated for this matrix yet.
        ## getting the matrix from the special object:
        matrix <- x$getMatrix()
        ## calculating the inverse
        inverse <- solve(matrix)
        ## caching in the special object that is held in the parent environment
        x$setInverse(inverse)
        inverse
}
