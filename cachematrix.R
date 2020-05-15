## Put comments here that give an overall description of what your
## functions do

## Creates a matrix for the inverse function

makeCacheMatrix <- function(x = matrix()){
        i <- NULL
        
        set <- function(matrix){
                x <<- matrix
                i <<- NULL
        }
        
        get <- function(){
                x
        }
        
        setInverse <- function(inverse){
                i <<- inverse
        }
        
        getInverse <- function(){
                i
        }
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse of the matrix created in previous function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getInverse()
        
        if(!is.null(mat)){
                return(mat)
        }
        
        data <- x$get()
        
        mat <- solve(data) %*% data
        
        x$setInverse(mat)
        
        mat
}
