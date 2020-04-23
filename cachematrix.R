## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Week 3 Assignment; week beginning April 23, 20120; GitHub user: pravashtiwari
### Objective : To write a  function that creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
### defining a matrix with a function with the purpose to cache its inverse
  inv <- NULL ## Initializing inverse matrix as null; the this will hold values of our inverse
  set<- function(y) { ## defining a function set to assign new value of matrix
  x <<- y           ####in parent environement######
  inv <<- NULL   ###In case of new matrix reset the inverse to NULL####
  }
  get<- function() x ## define the get fucntion - returns value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse ## assigns value of inv in parent environment
  getinverse <- function() inv  ## gets the value of inv where called
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
                                                                              ## to the functions with the $ operator
}
## Write a short comment describing this function
###This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  }
