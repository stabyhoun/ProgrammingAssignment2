## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. This assignment is to write a pair of functions that cache the inverse of a matrix.
##
## Write a short comment describing this function
##
## R Programming WK 3 Excercise, Programming Assignment 2
##
## Part 1: makeCasheMatrix
## 
## This function creates a special matrix object that can cache its inverse. Note: We are assuming that the matrix
## supllied is always invertible.
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
## R Programming WK 3 Excercise, Programming Assingment 2
##
## Part 2: cacheSolve
## 
## This function computes the inverse of the special matrix returned by makeCasheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
##

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'

        imatrix = x$getimatrix()
        ## check to see if the inverse has already been calculated
        if (!is.null(imatrix)) {
                ## If it is already done, just return imatrix
                return(imatrix)
        }
        ## If the inverse has not already been calculated, do so and return the results
        matrix = x$get()
        imatrix = solve(matrix, ...)
        x$setimatrix(imatrix)
        return(imatrix)
}
