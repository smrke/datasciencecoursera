## The two functions below are meant to be used together. 
## makeCacheMatrix takes as an argument a matrix, and returns a list 
## of functions (get, set, setinv and getinv) that can be used to act on that matrix
## if we store makeCacheMatrix as a variable, we can subset the variable
## to carry out the functions on the current value of the matrix

## the makeCacheMatrix function (modelled after the makeVector function) 
## creates the list of functions from the matrix, and because of lexical scoping
## allows us to perform these on the submitted matrix (or whatever it is eventually set to)


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## the cacheSovle function (modelled after the cacheMean function)
## takes the returned list of makeCacheMatrix as its argument
## first checks whether there is already an inverse by calling getinv()
## and if a non-null value exists, returns that with a printed message
## if there is not already a non-null value, it calculates it, stores
## the new value in makeCacheMatrix and returns it too

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

#final comment


