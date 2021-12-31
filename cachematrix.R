
##This function creates a special "matrix" object that can 
##cache its inverse.

makeCacheMatrix <- function(m = numeric()) {
  n <- NULL
  set <- function(y) {
    m <<- y
    n <<- NULL
    
    ##set() is included so that once an object of type 
    ##makeCacheMatrix() is created, its value can be changed 
    ##without initializing another instance of the object. 
    ##It is unnecessary the first time an object of type 
    ##makeCacheMatrix() is instantiated.
  }
  
  ##Within set() we use the <<- form of the assignment operator, 
  ##which assigns the value on the right side of the operator 
  ##to an object in the parent environment named by the object 
  ##on the left side of the operator.
  
  get <- function() m
  setinv <- function(inv) n <<- inv
  getinv <- function() n
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##Naming the list elements is what allows us to use the $ 
##form of the extract operator to access the functions by 
##name rather than using the [[ form of the extract operator, 
##, to get the contents of the vector.


##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already 
##been calculated (and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(makeCacheMatrix.object, ...) {
  n.local <- makeCacheMatrix.object$getinv()
  if(!is.null(n.local)) {
    message("getting cached data")
    return(n.local)
  }
  data <- makeCacheMatrix.object$get()
  n.local <- solve(data, ...)
  makeCacheMatrix.object$setinv(n.local)
  n.local # return the inverse
}

##Without cacheSolve(), the makeCacheMAtrix function is incomplete.
##cacheSolve() is required to populate and/or retrieve the inverse 
##from an object of type makeCacheMAtrix().
