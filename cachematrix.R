###
### These functions allow us to cache the potentially time consuming computation of computing 
### the inverse of a matrix.

### makeCacheMatrix() is used to create a storage object, then access that object using cacheSolve
### If the inverse has not yet been calculated then cacheSolve() calculates the inverse
### stores it in the object created by the call to makeCacheMatrix()
### and then returns the inverse.
### If the inverse has been calculated earlier then cacheSolve() fetches it and returns the inverse
### saving the computing time required to calculate the inverse again
###

###
### We use makeCacheMatrix to create the special "matrix" object that can cache it's inverse.  
###

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  ###
}


###
### cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
### If the inverse has already been calculated and the matrix has not changed, 
### cachesolve will retrieve the inverse from cache
###


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

###
### Matrices for testing gotten from discussion pages
### https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg
###
### Tested Using m1<- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
###
### Inverse should be 
###       [,1] [,2]
### [1,]    6    8
### [2,]    2    4
###
###
### and n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
###
### Inverse should be 
###
###      [,1] [,2]
### [1,]    3    7
### [2,]    1    5
###
###
