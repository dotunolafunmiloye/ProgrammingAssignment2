##makeCacheMatrix: function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mymatrix = matrix()) {
      
      #  mymatrix should be a  square matrix. 
      # makeCacheMatrix consists of 4 sub functions: a) Set the matrix, b) Set the matrix c) Get the inverse d) Set the inverse
      
      #Set matrix_status to NULL
      matrix_status <- NULL
      # SET matrix function. Use function to set the vector in the main function. 
      # When we set the function, variable matrix_status is also set to NULL
      setmatrix = function(value) {
            #  `<<-` assigns value to an object in an environment different from the current environment
            # Substitue vector mymatrix with y (the input) in the main function (makeCacheMatrix) 
            # Check if matrix is  square
            Numrow <- nrow(value)
            Numcol <- ncol(value)
            if ( Numrow == Numcol) {
                  #matrix is squre, continue processing
                  mymatrix <<- value
                  matrix_status <<-  NULL
            }else {
                  message ("Matrix is not a square, re-try with a square matrix.")
            }
      }
      
      # GET matrix function.Function returns the vector stored in the main function 
      getmatrix <- function() mymatrix
      
      # SETSOLVE: Function sets in inverse matrix; Store the value in the cache
      setsolve <- function(solve) matrix_status <<- solve
      
      # GETSOLVE: Function gets the  inverted matrix from the cache
      getsolve <- function() matrix_status

      # List() is used to store the functions in makeCacheMatrix. Such that when we assign makeCacheMatrix to an object,
      #the object has all the 4 functions.
      list(setmatrix=setmatrix, getmatrix=getmatrix, setsolve=setsolve, getsolve=getsolve)
         
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), cacheSolve retrieves the previously calculated value.

cacheSolve <- function(mymatrix, ...) {
      # retrieve matrix_status
      matrix_status <-  mymatrix$getsolve()
      # check if matrix_status has changed, if not, print message and recover saved state.
      if (!is.null(matrix_status)){
            message("Retrieving cached data .... ")
            matrix_status
      }
      # if matrix_status has changed, then recalculate, and update matrix_status
      matrix_data <- mymatrix$getmatrix()
      matrix_status <- solve(matrix_data, ...)
      mymatrix$setsolve(matrix_status)
      return(matrix_status)

}

