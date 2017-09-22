
## makeCacheMatrix contains child functions which allow the user to set either the parameters of a matrix or an inverse of a matrix. 
# cacheSolve function will either invert the matrix stored in 'makeCacheMatrix', or will retrieve the stored value of the inverse
# if it was defined('setInverse') in 'makeCacheMatrix'.  
#   Caching values or objects saves us from having to calculate the same thing multiple times and reduces
# computational effort.



## makeCacheMatrix is a parent function which contains multiple child functions: set, get, setInverse, getInverse. The set function
# has closures meaning its environment is closed off from the higher environment of the parent function. However the double arrow assignment
# ('<<-') used within the enclosed environment of 'set' searches for variables in the parent environment. So 'set' can still do the work,
# but can be modified by variables in the parent environment. And matrix of 'set' (x) can be retrieved by 'get'.

ls(environment(fn$set))
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## "inv" will retrieve the inverse matrix if already created, instead of creating it itself. This minimizes computation effort. 
## if "setInverse" wasnt already defined then this function will solve for the inverse. After setInverse creates its value it will 
## return "inv" and computation will complete after the first internal function. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setInverse(inv)
  inv
}


## Sample run:
fn <- makeCacheMatrix()
fn$set(matrix(1:4,2))
fn$get()
##     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

cacheSolve(fn)
##      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

