## R_Programming | Programming Assignment # 2 | RJS-C

## The functions in this R Script work together to reduce the amount of 
## machine work required by eliminating repetitive calculations of a matrix' inverse
## by allowing the user to access the cached value.


## Function:    makeCacheMatrix
## Input:       An Invertible Matrix
## Output:      A List of functions that can retrieve information about
##              a specific Matrix from which an inverse
##              can be calculated and stored for future retrieval (cached)

makeCacheMatrix <- function(x = matrix()) {
        invMtrx <- NULL
        
        set <- function(y) {
                x <<- y
                invMtrx <<- NULL
        }
        
        get <- function() x
        
        setInv <- function(inv) invMtrx <<- inv
        
        getInv <- function() invMtrx
        
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Function:    cacheSolve
## Input:       Special List of type makeCacheMatrix
## Output:      The Inverse of the Matrix stored in the argument passed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invMtrx <- x$getInv()
        
        if(!is.null(invMtrx)) {
                message("Cached Data Being Retrieved.")
                return(invMtrx)
        }
        
        origMtrx <- x$get()
        invMtrx <- solve(origMtrx)
        x$setInv(invMtrx)
        invMtrx
        
}
