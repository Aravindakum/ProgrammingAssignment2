
## Written three functions makeCacheMatrix,cacheSolve,Client
## makeCacheMatrix - Calls InverseMatrix for the first time and cache it internally for consecutive access from cache, invalidates the inverse when there is a change in matrix
## cacheSolve - Sample showcase that calls inverse 2 times..second time the inverse  gets it from cache..
## Client - Sample client access that creates the CacheMatrix Object and calls the cacheSolve to demostrate cache functionality


##---------------------------------------------------------------------------------

## Creates function that returns cache of matrix and inverse if called once..
## Otherwise it computes and cache the value for next time..
makeCacheMatrix <- function(m = matrix()) {
  inv_m <- NULL
  
  ## invalidate the inverse if there is a change in matrix
  set <- function(y) 
  {
    ## set the new value of matrix and clear the cached matrix inverse...
    m <<- y
    inv_m <<- NULL
  }
  
  get <- function() 
  {
    ## returns the inner matrix object..
    m
  }
  
  setinv <- function(inv) 
  {
    ## set the matrix inverse to cache object
    message("Setting the inverse matrix to Cache")
    inv_m <<- inv
  }
  
  ## Cache the matrix inverse after calculating for the first time..
  ## Uses Cache Inverse matrix instead of recalculating it..
  getinv <- function() { 
    ## If inv is null , it calculates the inverse and caches 
    ## else it returns the cached matrix inverse
    if(is.null(inv_m))
    {
      inv_m<-solve(m)
      setinv(inv_m)
    }
    else
    {
      message("getting Cahce inverse matrix")
    }
    inv_m
  }
  
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}

##---------------------------------------------------------------------------------
## Sample showcase that calls inverse 2 times..second time the inverse  gets it from cache..
cacheSolve <- function(cachematrix_object, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  
  ## First time in the call, the inverse is calculated , cache for future and returned
  message("--------------First time call to inverse the matrix-----------------")
  inv_matrix<-cachematrix_object$getinv()
  print(inv_matrix)
  ## Consecutive access the value is returned from the cache 
  message("-------------Second time call to inverse matrix---------------------")
  inv_matrix<-cachematrix_object$getinv()
  
  inv_matrix
}


##---------------------------------------------------------------------------------
## Sample client access that creates the CacheMatrix Object for a sample matrix and calls the cacheSolve to demostrate cache functionality
client <- function()
{
  A <- matrix( c ( 1, 2, 2, 1 ), nrow=2, byrow=TRUE)
  cacheMatrix<-makeCacheMatrix(A)
  cacheSolve(cacheMatrix)
}

