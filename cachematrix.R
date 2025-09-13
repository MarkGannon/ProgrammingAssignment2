## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix as a parameter and returns 
## a list of functions to get and set the inverse of the matrix
## The function attempts to set x and m in the parent environment
## via the <<- operator

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y       # store a new matrix
    m <<- NULL    # reset cached inverse
  }
  
  get <- function() x
  
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## Cache solve takes a list from the makeCacheMatrix
## It checks to see if there is already a value for the inverse 
## in the list.  If so, it returns that. 
## If not, it gets the original matrix, 
## Calculates the inverse, 
## Sets the inverse in the list, 
## and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	original_matrix <- x$get()
        m <- solve(original_matrix) 
        x$setInverse(m)
	m	
}
