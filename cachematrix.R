## makeCacheMatrix( matrix )
##
## create an object for handle matrix type and extend it with methods for cache the inverse:
## -        set : assign the matrix, clean the inverse cached
## -        get : get the matrix
## - setinverse : assign the inverse
## - getinverse : get the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve ( matrix, ...)
## it perform the Inverse Solve of a makeCacheMatrix's object anc cache the result
##
## If the function solve(Matrix) encounter some kind of error (not all matrix are invertible)
## it print the error and the result is not cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        tryCatch({
            i <- solve(data, ...)
            x$setinverse(i)
            i
        }, warning = function(w) {
          message("Warning: Inverse not cached")
          message( as.character(w) )
        }, error = function(e) {
          message("Error: Inverse not cached")
          message( as.character(e) )
        })
}


## HOW TO TEST
##
## **Create invertible matrix**
## > mymatrix <- makeCacheMatrix( matrix( c(1, 1, -1, 2), nrow=2, ncol=2) )
##
## **test makeMatrix object is working**
## > mymatrix$get() ##show the matrix
##
##  Expect to say:
##     [,1] [,2]
## [1,]    1   -1
## [2,]    1    2
##
## **solve and cache the solution**
## > cacheSolve(mymatrix)
##
##  Expect to say:
##           [,1]      [,2]
## [1,]  0.6666667 0.3333333
## [2,] -0.3333333 0.3333333
##
## **ask one more time for the solution**
## > cacheSolve(mymatrix)
##
##  Expect to say:
## getting cached data
##           [,1]      [,2]
## [1,]  0.6666667 0.3333333
## [2,] -0.3333333 0.3333333
##
## **Test for errors**
## > singularity <- makeCacheMatrix( matrix( 19:11, nrow=3, ncol=3) )
## > cacheSolve(singularity)
##
##  Expect to say:
## [1] "Error: Inverse not cached"
## [1] "Error in solve.default(data, ...): system is computationally singular: reciprocal condition number = 1.06537e-17\n"
