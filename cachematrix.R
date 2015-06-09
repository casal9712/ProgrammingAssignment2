## Below are two functions

## The first function, makeCacheMatrix, creates a special "matrix", which contains
## a list of 4 functions to perform the following:
## 1. set the value of the matrix (need to be invertible)
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
#create a place holder for the cache value
        m <- NULL	

#this function set new value(y) to x, and reset the place holder value, both at
#different environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

#this function return the current value of x, or the current matrix
        get <- function() x

#this function set a value(matrix) to m at a different enviornment
        setmatrix <- function(matrix) m <<- matrix

#this function return the current value of m
        getmatrix <- function() m

#this put the 4 functions as a list for use
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## The second function, cacheSolve, requires the output of makeCacheMatrix as
## the input. It would return the cached value of the inverse of the matrix that
## was input in the makeCacheMatrix function or compute the inverse for you if
## there was no cached value.

cacheSolve <- function(x, ...) {
#first get the value of the matrix through the getmatrix function from the
#makeCacheMatrix function
        m <- x$getmatrix()

#check whether the returned value is NULL or not, if it is not, then returned
#the stored(cached) matrix
        if(!is.null(m)) {
                message("GETTING CACHED DATA")
                return(m)
        }

#if the returned value is NULL, then it will get the matrix value input in the
#makeCacheMatrix function and compute the inverse of it, then cache it, and then
#return it
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}



## Example1(cacheSolve does the computation):
## x<-matrix(rnorm(100,100),10,10)
## inv<-makeCacheMatrix(x)
## cacheSolve(inv)

## Example2(cacheSolve does not do the computation and returned the cached value):
## y<-matrix(rnorm(100,100),10,10)
## inv<-makeCacheMatrix(y)
## inv$setmatrix(solve(y))
## cacheSolve(inv)