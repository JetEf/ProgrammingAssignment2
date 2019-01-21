## This function creates a matrix that can cache its inverse.

# This function contains:
# set the value of the matrix: set <-
# get the value of the matrix: get <-
# set the value of the inverse matrix: setmatrix <-
# get the value of the inverse matrix: getmatrix <-


makeCacheMatrix <- function(x = matrix()) {
      
      # variable: inv_matrix (= inverse matrix)
      inv_matrix <- NULL
      
      # set matrix with new function
      set <- function(y) {
            x <<- y
            inv_matrix <<- NULL
      }
      
      # get matrix
      get <- function() x
      
      # set inverse matrix
      setmatrix <- function(solve) inv_matrix <<- solve
      
      # get inverse matrix
      getmatrix <- function() inv_matrix
      
      # make list of four previous functions
      list( set = set,
            get = get,
            setmean = setmatrix,
            getmean = getmatrix
      )
}


## This function returns a matrix that is the inverse of 'x'
## If the inverse has already calculated, show message "getting cached matrix"
## and skips the computation.


cacheSolve <- function(x, ...) {
        
      # get matrix of function makecachematrix
      inv_matrix <- x$getmatrix()
      
      # return message when it's not null (when it does exist)
      if(!is.null(inv_matrix)) {
            message("getting cached matrix")
            return(inv_matrix)
      }
      
      # when there's no match:
      
      # get value of matrix
      data <- x$get()
      
      # calculate inverse
      inv_matrix <- solve(data, ...)
      
      # cache result in setmatrix of inverse
      x$setmatrix(inv_matrix)
      
      # return new matrix
      inv_matrix
}
