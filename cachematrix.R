## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve:  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##  should retrieve the inverse from the cache.

## NOTE: For this assignment, assume that the matrix supplied is always invertible

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix is a function that stores a list of functions
## makeCacheMatrix holds four functions:
##  (1)set: sets the matrix stored in the function makeCacheMatrix
##  (2)get: retruns the matrix stored in the function makeCacheMatrix
##  (3)set_im: sets the inverted matrix in the function makeCacheMatrix. Does not calculate the inverted matrix
##  (4)get_im: gets the inverted matrix in the function makeCacheMatrix. Does not calculate the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  ## sets the inverted matrix (im) to NULL
  im <- NULL
  
  ## sets the matrix stored in the function makeCacheMatrix
  set <- function(y) {
    x <<- y
    im <<- NULL
  }

  ## retruns the matrix stored in the function makeCacheMatrix 
  get <- function() x

  ## sets the inverted matrix in the function makeCacheMatrix. Does not calculate the inverted matrix 
  set_im <- function(inverted_matrix) im <<- inverted_matrix
  
  ## gets the inverted matrix in the function makeCacheMatrix. Does not calculate the inverted matrix
  get_im<- function() im
  
  ## store the 4 functions in the function makeCacheMatrix function
  list(set = set, get = get,
       set_im = set_im,
       get_im = get_im)
}


## cacheSolve:  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##  should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## get the matrix stored in makeCacheMatrix      
  im <- x$get_im()

  ## if the matrix stored in makeCacheMatrix is not NULL, we know that the inverse matrix has been calculated
  if(!is.null(im)) {
    message("getting cached data")
    ## return the inverse matrix stored in makeCacheMatrix
    return(im)
  }

  ## get the matrix stored in makeCacheMatrix
  data <- x$get()
  
  ## calculate the inverted matrix and assign the value to im
  im <- solve(data, ...)
  
  ## set the inverted matrix in makeCacheMatrix to the vale calculated now
  x$set_im(im)
  
  ## Return a matrix that is the inverse of 'x'
  im
}



##Test input
##(1)a <- makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2))
##(2)cacheSolve(a) #first time this command is run: should NOT print the message "getting cached data"
##(3)cacheSolve(a) #second time this command is run: should print the message "getting cached data"