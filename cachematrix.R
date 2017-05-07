### R Programming 
### Week 3 - Programming Assignment
### SEAN S - 05.01.2017
### DIR: /Users/Sean/Documents/R Files/ZS ADS/00 R Programming/Week 3/W3 Assignment"

## setwd('/Users/Sean/Documents/R Files/ZS ADS/00 R Programming/Week 3/W3 Assignment/ProgrammingAssignment2') 
## getwd()


## makeCacheMatrix | Accepts a matrix - and can perform 4 functions on the matrix it is provided 
## (set, get, setInverse, getInverse)

makeCacheMatrix <- function(x = matrix()) 
{
  
  matrixInverse <- NULL
  
  set <- function(y) 
  {
    x <<- y
    matrixInverse <<- NULL
  }
  
  get <- function() x

  setInverse <- function(inverse) matrixInverse <<- inverse

  getInverse <- function() matrixInverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve | Accepts a matrix, checks to see if the inverse has already been cached - if so -
## returns the cached value - if not cacluclated - calls the setInverse function to calculate

cacheSolve <- function(x) {
  matrixInverse <- x$getInverse() 
  if(!is.null(matrixInverse)) { 
    message("getting cached data")
    return(matrixInverse)
  }
  
  data <- x$get()  
  matrixInverse <- solve(data) 
  x$setInverse(matrixInverse)  
  matrixInverse               
}


### TEST CODE

## Sample run:
x = rbind(c(0, -1), c(-1/2, 0))
m = makeCacheMatrix(x)
m$get()

cacheSolve(m)
cacheSolve(m)
