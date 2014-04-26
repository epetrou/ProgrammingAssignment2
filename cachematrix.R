## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
        {   xinv<-NULL
                set <- function(y) {
                    x <<- y
                   
            }
            get <- function() x
            setinverse <- function(solve) xinv <<-solve
            getinverse <- function() xinv
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        y<- x$getinverse()   # x matrix's cache         
        if(!is.null(y)) {           #if there is the inverse
                message("getting cached data") 
                return(y)                #return the cache, i.e. invers, no computation needed
        }
        data <- x$get()             #if there's no cache
        y <- solve(data, ...)       #compute
        x$setinverse(y)             #save the result  x's cache
        y                           #return the result
}
