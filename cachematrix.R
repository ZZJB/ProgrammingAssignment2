#modified Jan 24, 2015

#Based on the outline from the vector examples
#Given the makeCacheMatrix function, assume x is an invertable matrix
makeCacheMatrix <- function(x = matrix()) {
 
#initialize m and create set function y with x (matix) and m (inverse matrix)
   m <- as.matrix(NULL) 
   set <- function(y=matrix()){
   x <<- as.matrix(y)
   m <<- as.matrix(NULL)
 }
 
 #create get function 
 get <- function() x
 
 #create setinvmatrix function to solve for inverse of matrix
 setinvmatrix <- function(solve) m <<- solve
 
 #create getinvmatrix function to return cached matrix 
 getinvmatrix <- function() m
 
 #set up list with set,get, setinvmatrix and getinvmatrix functions
 list(set=set, get=get, setinvmatrix=setinvmatrix, getinvmatrix=getinvmatrix)
 }
 
#given cacheSolve function with arguments passed from makeCacheMatrix
cacheSolve <- function(x= matrix(), ...) {
 
 #if m exists then return m as the inverse of the matrix 
     m <- x$getinvmatrix() 
     if(!is.null(m)){
       message("Returning cached data...")
       return(m)
     }
 #if m is null then calculate the inverse of the matrix and cache the value
     matrix <- x$get
     m <- solve(matrix, ...)
     x$setinvmatrix(m)
     return(m)
}

