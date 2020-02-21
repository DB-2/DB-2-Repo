## Programming assignment 2 - cachematrix
## Date: 20-Feb-20
## Author: DBarry
## Purpose: The general purpose of the assignment is to write some R functions that enable 
##          computed values be saved in cache(memory) allowing them be retrieved  
##          for re-use at a later stage in the process.
##          This avoids having to re-calculate the same value again and again in what are potentially
##          time-consuming computations.
##          The computation result to be cached in this case is the inverse of an invertible matrix
## 
## Function Name:  makeCacheMatrix 
## Function Purpose: The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## Function Arguments:1, x of type Matrix
## Function Return: The function returns a list with named List elements matching the makeCacheMatrix 
##                  Object getter and setter method functions. These methods allow direct access to each
##                  makeCacheMatrix objects instance environment and can be called as you would access a
##                  "named" list element i.e. using object@ne rather than object[list element number]
##
##                 sample call: tm<-makeCacheMatrix(matrix(c(4,2,7,6),nrow=2,ncol=2))
##     
## Function Description: The makeCacheMatrix contains the following object methods/functions 
## 1. set - when called,this allows manipulation and (re-)setting of the input matrix whose inverse is to be found
##          within its parent object environment
##          An initial call to makeCacheMatrix will populate the input matrix x which can be changed subsequently by this 
##          function for a new input matrix without creating a separate makeCacheMatrix object instance if required. 
##          This helps with memory management. It will also re-initialise the cached inverse matrix stored in im variable, which is used 
##          by the cacheSolve function below to drive retireval of the inversed matrix value from cache or to perform a new matrix
##          inversion on a new matrix that has been input.
## 2. get - when called this returns the values of the input matrix   
## 3. setinvm - when called this will set the cache (im) with the calculated inversed matrix storing it outside the setinvm function
##          environment in the parent makeCacheMatrix object environment
## 4. getinvm - when called this will get the inversed matrix value that has been stored in cache (im)
#### ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
##  
## Function Name: cacheSolve
## Function Purpose:     This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
##                       If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` 
##                       will retrieve the inverse from the cache.
## Function Arguments:1 to many, x of type makeCacheMatrix object and additional arguments to be transferred to Solve callif required
## Function Return: The function returns either the cache value of the inverse matrix if it has already been calculated or
##                  calculates the inverse and returns this value if not
##           sample call : cacheSolve(tm), where tm is a makeCacheMatrix 
##
##Function Description:The function is called with a makeCacheMatrix object as input. The cache value of this input object
##          is retrieved and if it is not empty the value is returned by the function without having to re-calculate the inverse.
##          if the cache is empty for the input object the input matrix is obtained from the object environment using the getter
##          method and the inverse calculated. This calculated value is then saved/assigned to cache using the object's cache setter   
##          function.          
##          Either the cached value or calculated value is returned by the function
##

## Function Description: The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

# Initialise cache variable
im <- NULL
# Declare setter method function to set the input matrix value x to input value y and re-initialise cache variable
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
 # Declare getter method to return the value of the input matrix to be inversed
  get <- function() x
 # Declare setter method to assign the inversed matrix to cache. the inversed matrix is the result of the Solve function in cacheSolve
  setinvm <- function(invm) im <<- invm  # Super assignment operator assigns the value to cache in the parent object environemnet
 # Declare getter method to get the value from cache,  im is available in the parent object environemnt
  getinvm <- function() im
  
 # Function returns a List with named elements matching the Object getter and setter method functions allowing them be called
 # by object / list named element 
  list(set = set, get = get,
       setinvm = setinvm,
       getinvm = getinvm)
}

## Function to compute the inverse of an invertible "matrix" or return it from cache if already calculated

cacheSolve <- function(x, ...) {
 # get the inversed matrix value from the makeCacheMatrix x parent object cache and assin it to cacheSolve local variable also called im
  im <- x$getinvm()
 # the value is not empty then it the cache has alreay been populated with th einverse matrix so return this inverse matrix
  if(!is.null(im)) {
    message("getting cached matrix data")
    return(im)
  }
 # otherwise get the input matrix value to be inversed
  data <- x$get()
 # assign the inverse to local variable im
  im <- solve(data, ...)
 # also assign the calculated inverse to cache using the makeCacheMatrix object setter method setinvm
 x$setinvm(im)
 # Finally return the local variable value im which contains the calculated inverse value from the Solve call 
  im 
}
