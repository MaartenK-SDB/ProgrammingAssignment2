## The following functions take matrixes and inverts them, saving matrix and the inverse matrix (iMatrix) to cache.
## The structure follows similar logic to the example mean function
## makeCacheMatrix takes an input matrix, and sets and retrieves the base matrix
## setiMatrix and getiMatrix are used by cacheSolve to calculate and retrieve previous iMatrixes

makeCacheMatrix <- function(x = matrix()) {
  m <<- NULL
  set <- function(y){ #Sets a matrix, technically redundant due to initial call but useful for documentation
    x <<- y
    m <<- NULL
  }
  get <- function() x #retrieves the base matrix
  setiMatrix <- function(iMatrix) m <<- iMatrix #sets the iMatrix, gets called in cacheSolve
  getiMatrix <- function() m #retrieves the cached iMatrix
  list(set = set, get = get,
       setiMatrix = setiMatrix, 
       getiMatrix = getiMatrix) #list is used to set the function, allowing for x$calls
}


## cacheSolve takes a makeCacheMatrix object as input, checks the cache, 
## and if empty calculates the iMatrix

cacheSolve <- function(x, ...) {
  m <- x$getiMatrix() #Retrieves iMatrix in cache
  #if no iMatrix present, the next part gets skipped
  if(!is.null(m)){
      message("getting cached iMatrix")
      return(m) #
  }
  basematrix <- x$get() #Retrieves matrix
  m <- solve(basematrix, ...) #Calculates inverseMatrix
  x$setiMatrix(m) #Sets the iMatrix to cache
  m
}




