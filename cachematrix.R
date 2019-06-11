##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(var1 = matrix()) {
        inv <- NULL
        set <- function(var2){
                var1 <<- var2
                inv <<- NULL
        }
        get <- function() var1
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## Return a matrix that is the inverse of 'var1'
cacheSolve <- function(var1, ...) {
       
        inv <- var1$getInverse()
        if(!is.null(inv)){
                message("Get Cached Data")
                return(inv)
        }
        data <- var1$get()
        inv <- solve(data)
        var1$setInverse(inv)
        inv      
}