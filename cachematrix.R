## Put comments here that give an overall description of what your
## functions do

# create a special "matrix", which is really a list containing a function to
# set/get value of the matrix, set/get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

# calculate the inverse of the special "matrix" created with the above function
# however, it first checks to see if the inverse has already been calculated
# if so, it gets the inverse from the cache and skips the computation;
# otherwise, it calculates the inverse and set the value of inverse in the cache via the setinv function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

# # code for testing
# a <- makeCacheMatrix()
# mat <- matrix(c(1,3,2,4,2,1,0,3,2),3,3)
# a$set(mat)
# a$get()
# cacheSolve(a)
# cacheSolve(a)
