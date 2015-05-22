## Two functions for creating and using matrix inversion cache

## initializes matrix invertion cache 
## returns either inverted matrix, or NA if matrix can't be inverted: 

makeCacheMatrix <- function(x = matrix()) {
  
  inv_result<-tryCatch({solve(x)},
                       error = function (err){print("non-invertible matrix!"); return(NA)})
  if (class(inv_result)!="matrix") {
    return (NA)}
  else {
    return (inv_result) 
  }
}


## Return a matrix, that is the inverse of 'matrix_object'
## Stores inverse value in cache_object
## Tests if currently stored inverse is correct by calculating 
## determinant of matrix x inverse multiplication result
## 

cacheSolve <- function(matrix_object, cache_object) {
  CorrectInversion <- function (m1, m2) {
    if (!(is.matrix(m1) && is.matrix(m2) && dim(m1) == dim(m2))) {
      return (FALSE);
    }
    return (det(m1 %*% m2) == 1);
  }
  
  if ((!is.na(cache_object)
      && CorrectInversion(matrix_object,
                          cache_object))) {
    return (cache_object)
  } else {
    inv_result<-tryCatch({solve(matrix_object)},
                         error = function (err){print("non-invertible matrix!"); return(NA)})
    if (class(inv_result)!="matrix") {return (NA)}
    
    eval.parent(substitute(cache_object<-inv_result))
    return (inv_result)
  }
}
