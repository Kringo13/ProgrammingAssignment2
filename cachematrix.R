## Function to demonstrate Lexical Scoping
## REturn cached value of matrix inverse, if exists

## create special vector of matrix inverse using gets and sets

makeCacheMatrix <- function(x = matrix()) {
  
    minv <- NULL
    set <- function(y) {
      x <<- y
      minv <<- NULL
    }
    get <- function() x
    setminv <- function(solve) minv <<- solve
    getminv <- function() solve
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
  
}


## Calculates inverse of MakeCacheMatrix.  If already exists, replace with known values from Globale Envirn.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getminv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setminv(minv)
  minv
}
