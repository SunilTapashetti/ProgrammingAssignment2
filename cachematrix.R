## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL             ## Set inv value to NULL indicating that
                              ## the input matrix has changed
        
        set<-function(y){     ## set the value of matrix and valiue of
                              ## the inverse = NULL
                x<<-y
                inv<<-NULL
        }
        get<-function(){x}    ## Get the value of the Matrix 
        getinv<-function(){inv} ## Get the Value of the inverse
        setinv<-function(inverse){inv<<-inverse} ## Set the value of the inverse
        list(get=get,set=set,setinv=setinv,getinv=getinv) ## Return List of 
                                                          ## functions

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getinv()    ##get value of inverse if available 
        if(!is.null(inverse)){
                message("getting inverse from Cache")
                return(inverse)                
        }
        mat<-x$get()           ## Get matrix for calculating inverse
        inverse<-solve(mat)
        x$setinv(inverse)
        inverse         ## Return the calculated inverse value        
}
