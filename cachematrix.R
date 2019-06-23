## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    #variable inverse is the inverse matrix
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(i) {
        inverse <<- i
    }
    getinverse <- function() {
        inverse
    }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
# computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # 1.Determin if the inverse has already been calculated
    # (and the matrix has not changed), then retrieve the inverse from the cache.
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # 2.Get a matrix from "makeCacheMatrix"
    data <- x$get()
    # 3.Computes the inverse of the matrix 
    i <- solve(data, ...)
    x$setinverse(i)
    i
}


# Define a matrix
# m <- matrix(2:5,2,2)
# initialized object testy
# myListOfFun <- makeCacheMatrix()
# myListOfFun$set(n)
# myListOfFun$get()
# myListOfFun$getinverse()

# cacheSolve(myListOfFun)
# newi<-cacheSolve(myListOfFun)

