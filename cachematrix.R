## This R script file has been prepared using the templates provided by Roger Peng
## The script contains two functions
## cacheSolve & makeCacheMatrix
### Further comments in-line of the function code.

## makeCacheMatrix --> This function achives the following
## 1.Creates a list containing the functions get,set,getinverse & setinverse
## 2.this function by itself provides little value, it has to be executed along with cacheSolve
## as cacheSolve determines if the inverse of the original matrix exists in memory(cahced) or not

makeCacheMatrix <- function(x = matrix()) {
  ## Here we initialize inv (retrun value) to NULL
  inv <- NULL
  ## set has two deliverables, initialize x and reset inv so that the correct cached value is 
  ## returned for every invocation with a new x value(function argument passed at invocation)
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get returns the value stored in x
  get <- function() x
  ## setinverse initializes the cached value
  setinverse <- function(inverse) inv <<- inverse
  ## getinverse returns the cached value
  getinverse <- function() inv
  ## a list is retuned with the functions created above(representing the behaviour) along with the environment
  ## which are the variables within the function(representing the state)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve --> This function checks if the inverse of the matrix (as produced by the R function solve())
## exists in the cache(parent environment), if yes then it returns the cached value, else it calculates the inverse 
## using the R function solve()

cacheSolve <- function(x, ...) {
### initialize the variable inv within cacheSolve using getinverse function 
  inv <- x$getinverse()
## if the value is not null then print message and return cached value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
## This step is required to isolate the original value for x as at this point x is a list
## Therefore solve(x,...) returns an error.
  data <- x$get()
## Calcuate the inverse using solve()  
inv <- solve(data, ...)
## set the inverse (using setinverse) for future invocations when the value for x is unchanged
x$setinverse(inv)
## return inverse, message is optional
message("inverse had to be calculated")
inv
}
