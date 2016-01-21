##The setMatrixValue method takes in a matrix argument from outside the current environment for assignment to an object. getMatrixValue can be called to return that object. setMatrixInv is similar to setMatrixValue, except it assigns a value to a separate object. getMatrixInv returns the inverted matrix if called. makeCacheMatrix takes a matrix object as input and returns the previous functions.
makeCacheMatrix <- function(x = matrix()){
    i <- NULL
    setMatrixValue <- function(y){
        x <<- y
        i <- NULL
    }
    getMatrixValue <- function() x
    setMatrixInv <- function(z) i <<- z
    getMatrixInv <- function() i
    list(setMatrixValue = setMatrixValue, getMatrixValue = getMatrixValue, setMatrixInv = setMatrixInv, getMatrixInv = getMatrixInv)
}


##Requires a matrix that must be invertible as input. The function calls the inverse from the cache using the getMatrixInv method; if present, it returns the inverse. If not present, it computes the matrix itself using solve and sets the inverse in the cache using the setMatrixInv method.
cacheSolve <- function(x,...){
    i <- x$getMatrixInv()
    if(!is.null(i)){
        return(i)
    }
    calcInv <- x$getMatrixValue()
    i <- solve(calcInv,...)
    x$setMatrixInv(i)
    i
}
