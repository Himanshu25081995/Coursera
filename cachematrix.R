## makeCachemtrix function create a list haing four functions which have following functions:
## 1. Set the value of matrix
## 2. get the value of matrix
## 3. Set the inverse of matrix
## 4. Set the inverse of matrix


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


## CacheSolve function calculate the inverse of above function. 

cacheSolve <- function(x, ...) {
  ##First it check if inverse of matrix is calucated or not. if yes than it return inverse of a Matrix.
  ##Otherwise it compute the inverse of matrix and set the inverse of a matrix through setInverse function.        
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
