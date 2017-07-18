## Function makeCacheMatrix creates a Matrix Object to cache tht inverse.


## We declare the makeCacheMatrix as n function of 'X', initiaze 'inv' as null which is the inverse matrix of x
## We thereafter declare 'set' as function of 'y' and declare 'x' as a function of 'y' in GlobalEnv and initialize 'inv'
## similarly we also declare 'get' as a function of 'x', declare 'setinverse' as function on 'inv' while declaring 'inv' to 'inverse' (inverse matrix of 'x' in globalenv
## declare 'getinverse' as function, getting inverse matrix 'inv'
## finally list the four elements viz set, get, setinverse and getinverse while defining thier value to the respective functions


makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL 
    set <- function(y) { 
        x <<- y 
        inv <<- NULL 
	} 
     get <- function() x 
     setinverse <- function(inverse) inv <<- inverse 
     getinverse <- function() inv
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
} 



## Function "cacheSolve" works out the inverse of the matrix resolved by the "makeCacheMatrix" function. 
## the result is primarily retrieved from the cache if the input matrix is same and already deduced.

## Step 1 would be to declare 'cacheSolve' as function of 'x' that will return inverse of 'x'
## Next step is to retrieve from "getinverse" into inv (Inverse Matrix of X)
## if inv is not NULL then retrieving value is already computed hence return the cached value for inv
## Else get the data from matrix 'x' computing inv as inverse matrix of x
## finally declare the value ov inv as 'setinversei in cache and return the value of 'inv'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse() 
   	if(!is.null(inv)) { 
      	message("getting cached data.") 
        return(inv) 
    } 
    data <- x$get() 
    inv <- solve(data) 
    x$setinverse(inv) 
    inv 
} 

