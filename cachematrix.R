###############################
##  created by Trevor
###############################
## the folowing two functions are a pair of functions
## that cache and solve the inverse of matrix's
##
##
## makeCacheMatrix function creates a list of 4 
## seperate functions. They are:
## 1) set - sets the value of the matrix, will change
##          it if necessary, and will set the inverse 
##          to null
## 2) get - gets the matrix from cache
## 3) setinv - sets the value of the inverse matrix
## 4) getinv - gets the inverse matrix from cache
makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse matrix value as NULL
  invmat <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse matrix
  setinv <- function(solve) invmat <<- solve
  
  #get the value of the inverse matrix
  getinv <- function() invmat
  
  # list of the funtions 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve function first checks to see if the inverse
## matrix has been solved before.  If it has, it will
## retrieve it from cache and return it.  If the 
## inverse has not been calculated, it will calculate
## it, set it to cache for future reference, and finally
## return the inverse matrix.
cacheSolve <- function(x, ...) {
  # first we check if the inverse has already been solved
  # if it has we return it from cache
  invmat <- x$getinv()
  if(!is.null(invmat)){
    message("retrieved inverse Matrix from cache ...")
    return(invmat)
  }
  
  # if the matrix has not been solved then we go ahead 
  # and solve it, store it, and return it
  message("Solving inverse Matrix ...")
  # this will solve it and then store it
  x$setinv(solve(x$get()))
  # finally we return it
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
## a <- makeCacheMatrix(m)
## cacheSolve(a)
##
## this should return a matrix like the one below:
## Solving inverse Matrix ...
## [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1
##
## as second re-run of cacheSolve(a) should demonstrate
## that it is pulling from cache:
## cacheSolve(a)
## retrieved inverse Matrix from cache ...
## [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1
##
##
## for larger matrix's use:
## using 6x6
## M<-matrix(c(sample(6),sample(6),sample(6),sample(6),sample(6),sample(6)),6,6)
##