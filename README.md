## The two functions have the ability to store the matrix and its cached version. First, as per 
##  the instruction, we assume that the matrix m is invertible. 
## The makeCacheMatrix stores the matrix and its inverse. It also is an easy and efficient way to ## recall the matrix and its inverse. 
## The cacheSolve recalls the inverse of a matrix if it has already been computed. If the inverse ## hasn't been calculated previously, then it solves for it and stores it in cache



makeCacheMatrix<- function(x = matrix((){
        inv<-NULL ## this is where the inverted matrix would be
        set<-function(y){
            x<<-y
        }
        get<- function()x
        setinversion <- function(i) inv<<-i
        getinversion <- function() inv
        list(set=set, get=get,
            setinversion = setinversion,
            getinversion = getinversion)
}
## This checks if theres an inverted matrix already. If there isn't, it solves for one and stores ## it in the cache.

cacheSolve <- function(w,...){
        inv <- w$getinversion()
        if(!is.null(inv)){
                message("using cache data")
                return(inv)
        }
        matrix <-w$get()
        inv <- solve(matrix,...)
        w$setinversion(inv)
        inv
}


