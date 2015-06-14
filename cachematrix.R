###############################
##  created by Trevor
###############################
## the folowing two functions are a pair of functions
## that cache and solve the inverse of matrix's
##
## makeCacheMatrix function creates a list of 4 
## seperate functions. They are:
## 1) set - sets the value of the matrix, will change
##          it if necessary, and will set the inverse 
##          to null
## 2) get - gets the matrix from cache
## 3) setinv - sets the value of the inverse matrix
## 4) getinv - gets the inverse matrix from cache
## this function will not calculate the inverse.  It
## only stores it.
makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinv <- function(solve) invmat <<- solve 
  getinv <- function() invmat
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function first checks to see if the inverse
## matrix has been solved before.  If it has, it will
## retrieve it from cache and return it.  If the 
## inverse has not been calculated, it will calculate
## it, set it to cache for future reference, and finally
## return the inverse matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinv()
  if(!is.null(invmat)){
    message("retrieved inverse Matrix from cache ...")
    return(invmat)
  }
  message("Solving inverse Matrix ...")
  x$setinv(solve(x$get()))
  return(x$getinv())
}

##########################
## Testing info
#########################
##
## For testing and to create a quick matrix m use:
## m <- matrix(c(-1, -2, 1, 1), 2,2)
## this will create a matrix that looks like:
## [,1] [,2]
## [1,]   -1    1
## [2,]   -2    1
##
## the inverse of this matrix is:
## [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1
##
## for larger matrix's use:
## using 6x6
## M<-matrix(c(sample(6),sample(6),sample(6),sample(6),sample(6),sample(6)),6,6)
##