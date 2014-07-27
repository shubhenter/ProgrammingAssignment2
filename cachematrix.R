# makeCacheMatrix is supposed to take a matrix as an input
# and define the values to be returned for 4 functions: 
#         i. set --> setting a new matrix means that inverse has to be calculated
#                    and hence should be set to NULL.
#         ii. get --> it merely returns the matrix as it is
#         iii. setInverse --> it caches the value of variable i for future refernce
#         iv. getInverse --> it retrives the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) i <<- Inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}




# cacheSolve checks if an inverse is stored in the cache.
# If yes, it returns the cache, else, 
# it calcuates the inverse, and calls the setInverse function to store 
# for the next call
# 
# Sample calls and the expected output:
# 
#         > M <- matrix(1:4, ncol=2, nrow=2)
#         > M
#                [,1] [,2]
#         [1,]    1    3
#         [2,]    2    4
#         > cacheM <- makeCacheMatrix(M)
#         > cacheSolve(cacheM)
#                 [,1] [,2]
#         [1,]   -2  1.5
#         [2,]    1 -0.5
#         > cacheM$get()
#                [,1] [,2]
#         [1,]    1    3
#         [2,]    2    4
#         > cacheM$getInverse()
#                [,1] [,2]
#         [1,]   -2  1.5
#         [2,]    1 -0.5
#         > cacheSolve(cacheM)
#         getting cached data
#                 [,1] [,2]
#         [1,]   -2  1.5
#         [2,]    1 -0.5
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}