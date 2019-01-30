## This function creates a vector containing a function to set and get the value of the vector 
## and set and get the inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <-NULL
      set<- function(y){
          x<<-y
          inv <<-NULL
        
      }
      get <-function()x
      setinverse <-function(inverse) inv <<-inverse
      getinverse <-function() x
      list (set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## This function calculates the inverse of a matrix of the vector created by makeCacheMatrix function.
## It will initially check if the matrix inverse calculation has been performed and if so it will 
## get the matrix inverse data from the cache using the setinverse function. If the matrix inverse 
## calculation hasn't previously been performed this function will carry out the calcuation and 
## will set the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
          inv <-x$getinverse()
          if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
          }
          data <-x$get()
          inv <- solve(data, ...)
          x$setinverse(inv)
          inv
  
        ## Return a matrix that is the inverse of 'x'
}
