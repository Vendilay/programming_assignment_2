# Following two functions, makeCacheMatrix and cacheSolve, are to be used to
# cache the inverse of a matrix.

# In the following function the arguments of the list does;
# set, sets the matrix value
# get, gets the matrix value
# setinverse, sets the inverse matrix value
# getinverse, gets the inverse matrix value

makeCacheMatrix <- function(x = matrix())
  {
  i <- NULL
  set <- function(y)
      
    {
    x <<- y 
    i <<- NULL  
    }  
    
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }


# The following function returns the inverse of matrix.

cacheSolve <- function(x, ...)
  {
  i <- x$getinverse()
  if (!is.null(i))  
      
    {  
    message("Getting Cached Data") 
    return(i) 
    }
    
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i 
  }

# Example
# The first expression creates a randomized 2x2 matrix.

x <- matrix(rexp(4, rate=.1), ncol=2)
y <- makeCacheMatrix(x)
z <- cacheSolve(y)

# An example for a random matrix;
#           [,1]       [,2]
# [1,]  3.329605  0.4603402
# [2,] 22.225423 14.1100545

# Inversion of said matrix using previously written functions;
#            [,1]        [,2]
# [1,]  0.3839507 -0.01252638
# [2,] -0.6047791  0.09060235
