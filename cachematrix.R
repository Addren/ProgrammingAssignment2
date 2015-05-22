## Two functions for creating and using matrix inversion cache

## initializes matrix enclosure object

makeCacheMatrix <- function(x = matrix()) {
  inv_result<- NA
  
  set <- function(mvalue) {
    x<<-mvalue
    inv_result<<- NA
  }
  get <- function() {return (x)}
  setinverse <- function(value) inv_result <<- value
  getinverse <- function() inv_result
  list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
  
}


## Return a matrix, that is the inverse of a value stored in 'matrix_object'
## Tests if currently stored inverse is correct by calculating 
## determinant of matrix x inverse multiplication result
## 

cacheSolve <- function(matrix_object, ...) {
  CorrectInversion <- function (m1, m2) {
    if (!(is.matrix(m1) && is.matrix(m2) && dim(m1) == dim(m2))) {
      return (FALSE);
    }
    return (det(m1 %*% m2) == 1);
  }
  
  inv_result <- matrix_object$getinverse()
  data <- matrix_object$get()
  if ((!is.na(inv_result)
      && CorrectInversion(data,
                          inv_result))) {
    message("used cache data")
    return (inv_result)
  } else {
    inv_result<-tryCatch({solve(data)},
                         error = function (err){print("non-invertible matrix!"); return(NA)})
    if (class(inv_result)!="matrix") {return (NA)}
    matrix_object$setinverse(inv_result)
    return (inv_result)
  }
}
