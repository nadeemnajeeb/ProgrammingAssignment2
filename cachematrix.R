# Caching is the process of storing the matrix data in a temporary storage
# The program's roadmap is as follows:
# Input matrix
# Cache the matrix
# Invoke the function to compute its inverse
# The cacheSolve() checks if the matrix is in the environment other than itself
# If found, it simply returns the cache copy without recomupting the result
# If not found in cache, it computes the inverse, stores it in the cache and returns it


## makeCacheMatrix() caches a given matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Input: A reversible matrix
  ## Output: The inverse of the reversible matrix
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(solve) inverseMatrix <<- solve
  getInverseMatrix <- function() inverseMatrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

# The cacheSolve() checks if the matrix is in the environment other than itself
# If found, it simply returns the cache copy without recomupting the result
# If not found in cache, it computes the inverse, stores it in the cache and returns it

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)) {
    message("Matrix already cached. Fetching cached data")
    return(inverseMatrix)
  }
  temp <- x$get()
  inverseMatrix <- solve(temp, ...)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}
