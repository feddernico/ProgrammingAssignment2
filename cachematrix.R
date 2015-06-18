# calculate the inverse of a matrix can be a heavy operation
# so it can be important to cache the inverse of a matrix
# and not calculate it over and over again

# makeCacheMatrix gets a matrix as argument
# and permits to the user to: 
# - set the value of the matrix.
# - get it back.
# - set the value of inverse of that matrix.
# - get it back.
makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv_m <<- inv
  getinv <- function() inv_m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# the cacheSOlve function permits to the user 
# to calculate the inverse of the matrix given in input, 
# if it's the first time ever and it stores it for the future. 
# If it's not the first time it displays the message: 
# "getting cached data", and it retrieves the previously computed matrix.
# The function also assumes that the given matrix is invertible.
cacheSolve <- function(x, ...) {
  inv_m <- x$getinv()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data)
  x$setinv(inv_m)
  inv_m
}