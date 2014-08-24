## coputate the matix inversion is costly.in this function,we cache the result,then we can
## use the result again and again quickly.

## Caching the Inverse of a Matrix
## eg:a<-makeCacheMatrix(matrix(c(1,2,12,13), nrow = 2, ncol = 2));cacheSolve(a)   

makeCacheMatrix <- function(x = matrix()) {

    ## This function creates a special "matrix" object that can cache its inverse.
    
    inv_matrix<-NULL
    
    set<-function(y){
        x<<-y
        inv_mtrix<<-NULL
    }
    get<-function()x
    set_inv<-function(inv) inv_matrix<<-inv
    get_inv<-function()inv_matrix
    list(set=set,get=get,
         set_inv=set_inv,get_inv=get_inv)
}



cacheSolve <- function(x, ...) {
   # This function computes the inverse of the special "matrix" returned
   #by makeCacheMatrix above. If the inverse has already been calculated 
   #(and the matrix has not changed), then the cachesolve should retrieve 
   #the inverse from the cache.
    inv_matrix<-x$get_inv()
    if(!is.null(inv_matrix)){
        message("getting cached data")
        return(inv_matrix)
    }
        matrix_data<-x$get()
        inv_matrix<-solve(matrix_data)
        x$set_inv(inv_matrix)
        inv_matrix
        ## Return a matrix that is the inverse of 'x'
}
