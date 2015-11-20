## Function  makeCacheMatrix generates the list of functions used by function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
 
        setMatrix <- function(matrix = matrix()){
                x <<- matrix
        }
 
        getMatrix <- function() x
 
        setInverse <- function(inverseMatrix = matrix()){ 
                inverse <<- inverseMatrix
        } 

        getInverse <- function() inverse
        ## The list below is returned by ths function. cacheSolve needs this list as an argument.
        list(get = getMatrix, set = setMatrix, getInverse = getInverse, setInverse = setInverse)
}



## cacheSolve : Inverts the matrix, when the inverse does not exist already in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		        ## Return a matrix that is the inverse of x
        ## Matrix assumed invertible.  
 
        if(!is.null(x$getInverse())){
                print("Matrix was already in the cache; do nothing.")
        }
        else {
	         print("Matrix was not cached hence inverse computed.") 
               x$setI(solve(x$get()))
	    }
 
        x$getInverse()
}

##Sample Run
## x <- matrix(1:9,3,3)
## x[2,2] <- 100     "This makes the matrix invertible"
## y <- makeCacheMatrix(x)
## z <- cacheSolve(y)
## [1] "Matrix was not cached hence inverse computed."
## > z
##             [,1]         [,2]         [,3]
## [1,] -0.747368421 -0.005263158  0.585964912
## [2,] -0.005263158  0.010526316 -0.005263158
## [3,]  0.252631579 -0.005263158 -0.080701754
##
## z <- cacheSolve(y)
## [1] "Matrix was already in the cache; do nothing."
##
##
