## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function takes a matrix x. It assignes the null value to the xinv, that is
## the valiable for the inverse. The set and get functions are defined to set and
## get the matrix. Then the setinverse calculates the inverse of the matrix with
## solve and getinverse is a function gets the inverse. Finnaly we put all this
## into a list that we can call with x$....


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
## The following function works as follows:
## x is the list that was created in the makeCacheMatrix
## From this list we assign the getiverse argument to y.
## In the if loop we examine is this cache is non-empty, i.e. the inverse
## of the matrix is already calculated, then the function just get's the 
## inverse matrix and returns it. Otherwise the inverse is calculated by calling
## get, applying solve  and  set the inverse with setinverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        y<- x$getinverse()            
        if(!is.null(y)) {           #if there is the inverse
                message("getting cached data") 
                return(y)                #return the cache, i.e. invers, no computation needed
        }
        data <- x$get()             #if there's no cache
        y <- solve(data, ...)       #compute
        x$setinverse(y)             #save the result  x's cache
        y                           #return the result
}
