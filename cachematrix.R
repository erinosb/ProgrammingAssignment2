####################################################################################
## Author: E O N
## Date: July 21, 2014
## Name: ProgrammingAssignment2
## Bugs: 
## To add:
####################################################################################


#Makes a list containing functions to set and get a matrix, set and get its inverse
makeCacheMatrix <- function(x = as.matrix(as.numeric())){
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(solve){
                m <<- solve
        }
        getinverse <- function() {
                m
        }
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#Makes a function to recall a cached inverse matrix, if it exists
#If it does not exist, compute the inverse
cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

# #Test makeCacheMatrix
# matrix_ex <- matrix(1:4, 2,2)
# matrix_ex
# cached_output <- makeCacheMatrix(matrix_ex)
# cached_output$get()
# cacheSolve(cached_output)
# cacheSolve(cached_output)
# 
# 
# matrix_ex <- matrix(3:6, 2,2)
# cacheSolve(cached_output)
