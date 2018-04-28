## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCachematrix <- function (x = matrix()){
        inv <- NULL
        set <-function(y) {
                x <<- y 
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function()inv
        list (set = get, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Write a short comment describing this function
# Inverse of cache matrix

cacheInverse <- function (x, ...){
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

c <- matrix(c(1,4,3,2), nrows = 2, ncols = 2)

e<- makeCachematrix(c)
e
f <- cacheInverse(e)
f