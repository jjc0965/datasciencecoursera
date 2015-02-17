makeCacheMatrix <- function(matrix = matrix()) {
  matrix_solve <- NULL  ##sets matrix_solve to NULL as a placeholder for a future variable
  set_matrix <- function(new_matrix) {  ##defines a function to set the matrix 'matrix' to a new matrix 'new matrix' and resets the inverse to NULL 
    matrix <<- new_matrix 
    matrix_solve <<- NULL
  }
  get_matrix <- function() {  ##returns the matrix 'matrix'
    matrix
  }
  set_matrix_solve <- function(new_matrix_solve) {  ##sets the inverse 'matrix_solve' to 'new_matrix_solve'
    matrix_solve <<- new_matrix_solve
  }
  get_matrix_solve <- function() {  ##returns the inverse 'solve"
    matrix_solve
  }
  list(set_matrix = set_matrix, 
       get_matrix = get_matrix,
       set_matrix_solve = set_matrix_solve,
       get_matrix_solve = get_matrix_solve)  ##returns the special list containing all of the functions just defined
}

cacheSolve <- function(matrix_obj, ...) {
  matrix_solve <- matrix_obj$get_matrix_solve()
  if(!is.null(matrix_solve)) {
    message("getting cached data")
    return(matrix_solve)  ##if the inverse is stored under the parameters "inverse of matrix is not NULL", return it
  }
  data <- matrix_obj$get_matrix()  ##if the inverse is not stored, this assignes to 'data' the matrix 'matrix_obj'
  matrix_solve <- solve(data, ...)  ##calculates the inverse and assigns it to 'matrix_solve'
  matrix_obj$set_matrix_solve(matrix_solve)  #stores the inverse 'matrix_solve' under the parameters "matrix matrix_obj'
  matrix_solve  ##returns 'matrix_solve
}


xx<-matrix(1:4,nrow=2, ncol=2)

solve_matrix<-makeCacheMatrix(xx)
solve_matrix$get_matrix()
cacheSolve(solve_matrix)
cacheSolve(solve_matrix)