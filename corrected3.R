## This is an assignment in R programming for lexical scoping
##The following function create an object that both stores a matrix and caches its inverse. 
## 

## This is me telling R to creates a special “matrix”, which is really a list containing a function to:

#set the value of the matrix

#get the value of the matrix

#set the value of the inverse

#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## In the next function, I tell R to calculate the inverse of the matrix from the function above "makeCacheMatrix". 
# Also, if the inverse has been calculated and the matrix remained unchanged, then R should perform "cacheSolve" by retrieving the inverse of cache.



cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## this is me testing the functions

#solve(da) 
da <- matrix(c(1,2,3,4),2,2)

ve <- makeCacheMatrix(da)
cacheSolve(ve)
#the inverse is outputed 
## this is what it looks like
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

cacheSolve(ve)
#the inverse is outputed 
## this is what it looks like
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
