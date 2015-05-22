## Two functions for creating and using matrix enclosure object with
## cache-stored matrix inversion result

## returns list with fields: 
##    "matrix" - main work matrix
##    "inversion" - for storing matrix inversion cache  

makeCacheMatrix <- function(x = matrix()) {
  Inversion = NA
  return (list(matrix=x, inversion=Inversion))
  
}


## Return a matrix, that is the inverse of 'matrix_object'
## Stores inverse value in matrix object "inversion" field
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
  
  if ((!is.na(matrix_object[["inversion"]]))
      && CorrectInversion(matrix_object[["matrix"]],
                          matrix_object[["inversion"]])) {
    return (matrix_object[["inversion"]])
  } else {
    inv_result<-tryCatch({solve(matrix_object[["matrix"]])},
                         error = function (err){print("non-invertible matrix!"); return(NA)})
    if (class(inv_result)!="matrix") {return (NA)}
    
    new_object <- list(matrix=matrix_object[["matrix"]], inversion=NA)
    new_object[["inversion"]] <- inv_result
    eval.parent(substitute(matrix_object<-new_object))
    return (new_object[["inversion"]])
  }
}
