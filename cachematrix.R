## Calculate inverse of a matrix and cache the result.
## The second call for the same matrix will return the result from the cache.

## Calculate the inverse of a matrix and store the original 
## and the inverse in cache variable

makeCacheMatrix <- function(x = matrix()) {
  cachedOriginalMatrix <<- x
  cachedInverseMatrix <<- solve(x)
  cachedInverseMatrix
}


## cacheSolve checks if x invserse is calculated already
## return the cached matrix, or calculate the inverse of the 
# new matrix, and return it
cacheSolve <- function(x, ...) {
        sameAsCache <- FALSE
        # check if the dimensions are the same
        if (sum(dim(x) == dim(cachedOriginalMatrix)) == 2) {
          # count the elements, that are not equal to the cached matrix
          if (sum(!(x == cachedOriginalMatrix)) == 0) {
            sameAsCache <- TRUE
          }
        }
        if (sameAsCache) {
          # Return cached matrix
          cachedInverseMatrix
        } else {
          # Calculate end return
          makeCacheMatrix(x)
        }
}
