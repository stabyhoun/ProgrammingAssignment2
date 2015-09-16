## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## R Programming WK 3 Excercise
##
## Part 1: makeCasheMatrix
## 
## This function creates a special matrix object that can cache its inverse
##

makeCacheMatrix <- function(x = matrix()) {

        ## Initialize variables
        imatrix = NULL
        ## Set the value of the matrix
        set <- function(y) {
                x <<- y
                imatrix <<- NULL
        }
        ## Get the value of the matrix
        get <- function() x
        ## Set the inverse of the matrix
        setimatrix <-function(solve) imatrix <<- solve
        getimatrix <- function() imatrix
        list(set=set, get=get, setimatrix=setimatrix, getimatrix=getimatrix)
}



## Write a short comment describing this function

##
## R Programming WK 3 Excercise
##
## Part 2: cacheSolve
## 
## This function computes the invers of the special matrix returned by makeCasheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from teh cache.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        imatrix = x$getimatrix()
        ## check to see if the inverse has already been calculated
        if (!is.null(imatrix)) {
                ## If it is already done, just return imatrix
                return(imatrix)
        }
        ## If the inverse has not already been calculated, do so and return the results
        mat.data = x$get()
        imatrix = solve(mat.data, ...)
        x$setimatrix(imatrix)
        return(imatrix)
}