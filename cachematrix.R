## This code calculates the inverse of a matrix and checks if the inverse has already
##been calculated then it gets the inverse from cache otherwise if there is a change in matrix
##then it calculates again


makeCacheMatrix <- function(x = matrix())
##x is a square matrix
  {
  ## This function returns a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##this list is used as the input to cacheSolve()
    mat_inverse<-NULL
    set=function(y)
      {
        x<<-y
        mat_inverse<<-NULL
      }
  get<-function() x
  setinv<-function(solve) mat_inverse<<-solve
  getinv<-function() mat_inverse
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}



cacheSolve <- function(x, ...) 
  {
##return inverse of origmal matrix to makeCachematrix function
  mat_inverse<-x$getinv()
  if (!is.null(mat_inverse))
    ##Check if inverse is already caluclated
    {
    message("getting cached data")
    return(mat_inverse)
    }
##Calculate inverse of matrix
        matrix<-x$get()
        mat_inverse<-solve(matrix,...)
        x$setinv(mat_inverse)
        mat_inverse
        ## Return a matrix that is the inverse of 'x'
}
