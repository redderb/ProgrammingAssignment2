## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL   #inverse property
        set <- function(y) {   #set the matrix
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,     #set inverse of the matrix
             getinverse = getinverse)     #get inverse of the matric
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        i <- x$getinverse()  #matrix inverse of x
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()   #create matrix
        i <- solve(data, ...)   #calculate inverse
        x$setinverse(i)   #set mtrix
        i
}

