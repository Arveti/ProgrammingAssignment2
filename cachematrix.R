
## Here I used makecache vector along with <<- operator in makeCacheMatrix function and as
## it can be used in other environment, it is used in cache solve function to check whether
## it is already been cached or not and here in this case it looks for an inverse of a matrix
## as here we made a matrix as an argument and finding its inverse using solve function and is
## here acheived by using list of function namely set,get,setmatrix,getmatrix function and set the
## variable vector to enable caching in different environment and to retrieve the matrices.

makeCacheMatrix <- function(x = matrix())
  {
  makecache<-NULL
  set<-function(y){
    x<<-y
    makecache<<-NULL
  }
  get<-function()
  { x
  }
  setmatrix<-function(solve)
  {
    makecache<<- solve
  }
  getmatrix<-function() makecache
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Here cacheSolve function takes the variable returned by makeCacheMatrix function as its
## argument and first gets the matrix from getmatrix() and verifies whether it is already
## cached and if so returns the cached matrix from other environment in which it is defined
## and if not already cached, it get the matrix,finds it inverse and set the matrix to the 
## makecache variable by calling setmatrix() function and finally returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  makecache<-x$getmatrix()
  if(!is.null(makecache)){
    message("Getting cached Version of data")
    return(makecache)
  }
  matrix <- x$get() 
  makecache<-solve(matrix, ...)
  x$setmatrix(makecache)
  makecache
}
