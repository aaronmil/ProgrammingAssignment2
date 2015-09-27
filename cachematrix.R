## Below are two functions that work in conjunction to invert a matrix.
## The functions use cache as an environment for storing and inverting
## matrix, so as to avoid the time-consuming operations necessary when
## storing the matrix within the environment of each function.

## makeCacheMatrix is a function where a matrix is input as an argument,
## then stored in cache as "x." In addition, the inverse of the
## matrix will be stored in cache as variable "m."

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function()m
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}


## cacheSolve attempts to retrieve from cache the inverted matrix "m." 
## If "m" is NULL, then cacheSolve will calculate the inverse of matrix
## "x"

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
