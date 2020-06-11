## caching the inverse of a matrix and using it next time instead of computing a new, since the "matrix" remains the same

## creating a special matrix which caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverz <- NULL
  nast <- function(a){
    inverz <<- NULL
    x <<- a
  
  }
  
  
  zis <- function() x
  setInv <- function(solveMatrix) inverz <<- solveMatrix
  getInv <- function() inverz
  list(nast = nast, zis = zis, setInv = setInv, getInv = getInv)

}


## computing the inverse of the matrix returned by makeCacheMatrix
# (if it hasn't been already calculated)

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  
  inverz <- x$getInv()
  
  if(!is.null(inverz)){
    
    return(inverz)
  }
  
  matice <- x$zis()
  inverz <- solve(matice)
  x$setInv(inverz)
  inverz
  
}


