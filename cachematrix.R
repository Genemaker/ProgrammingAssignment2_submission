## Together the following two functions will create
## a function to contain an inverse of a matrix
## and then compute the inverse if not already stored in cache.


## This function creates a special "matrix" object 
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL  ## instantiate i within function
        set <- function(y) {
                x <<- y
                i <<- NULL  ## instantiate i within parent environment
        }
        get <- function() x  ## declaring get function
        setinverse <- function(inverse) i <<- inverse  ## declaring setinverse function
        getinverse <- function() i  ## declaring getinverse function
        ## create a matrix with a list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If 
## the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        ## This if statement check is i is not null (meaning it is cached), if so it returns cached value
        if(!is.null(i)) {
                return(i)
        }
        ## If i is null the function calculates the inverse and returns i
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

cat("#############\n")
cat("#Example #1:#\n")
cat("#############\n\n")
## Declare a matrix (x)
x <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## Display matrix (x)
cat("x Matrix:\n")
print(x)
## Create a cache of the matrix (x) returning x1
x1 <- makeCacheMatrix(x)
## Solve matrix (x) -- create inverse
x2 <- cacheSolve(x1)
## Display inverse matrix (x2)
cat("\nInverse of x Matrix:\n")
print(x2)

cat("\n#############\n")
cat("#Example #2:#\n")
cat("#############\n\n")

## Declare a matrix (x)
x <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
## Display matrix (x)
cat("x Matrix:\n")
print(x)
## Create a cache of the matrix (x) returning x1
x1 <- makeCacheMatrix(x)
## Solve matrix (x) -- create inverse
x2 <- cacheSolve(x1)
## Display inverse matrix (x2)
cat("\nInverse of x Matrix:\n")
print(x2)

cat("\n#############\n")
cat("#Example #3:#\n")
cat("#############\n\n")
## Declare a matrix (x)
x <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
## Display matrix (x)
cat("x Matrix:\n")
print(x)
## Create a cache of the matrix (x) returning x1
x1 <- makeCacheMatrix(x)
## Solve matrix (x) -- create inverse
x2 <- cacheSolve(x1)
## Display inverse matrix (x2)
cat("\nInverse of x Matrix:\n")
print(x2)