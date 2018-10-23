## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are two functions that are used to create a special object that stores 
## a matrix and cache's its inversion.

## makeCacheMatrix creates a matrix, which is really a list containing a
## function to:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the matrix. However, it
## first checks to see if the inverse has already been calculated. If so, it
## gets the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the data and sets the value in the cache via the
## setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

## Example
## > x = rbind(c(1,2), c(2,1))    #create example matrix
## > x                            #print matrix to see if it is in line with 
##                                 expectations
##     [,1] [,2]
## [1,]    1    2
## [2,]    2    1
##
## > m = makeCacheMatrix(x)       #run function 1
## > m$get()                      #check if it is equal to "x", see above
##     [,1] [,2]
## [1,]    1    2
## [2,]    2    1
##
## There is no cache after first run:
## > cacheSolve(m)
##            [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
##
## Second run, the cached inversed matrix is loaded which is indicated by the
## message "getting cached data"
## > cacheSolve(m)
## getting cached data
##           [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
