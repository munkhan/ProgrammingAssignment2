
#This function creates a special "matrix" object that can cache its inverse.
# If the matrix exists in cache retrive from cache instead of creating it again
cheMatrix <- function(x = matrix()) {

        inv<-NULL
        set<-function(y){
                c_x<-get()
                if(identical(c_x,y)){
                        message("Matrix exists in cache")
                }
                else  {    
                        x<<-y
                        inv<<-NULL
                }
        }
        
        get <- function() x
        setinverse <- function(inverse) inv<<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse= getinverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix
#above. If the inverse has already been calculated , 
#then the cachesolve retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Inverse found in cache")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

