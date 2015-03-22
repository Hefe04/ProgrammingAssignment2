##Creates a matrix that is saved within a kind of "cache" and when trying to solve
##it will look into the "cache" to retrieve the data or calculate it if it is not there and return the inverse of the matrix

## 

makeCacheMatrix <- function(x = matrix()) {
		##Store the matrix variable as NULL	
		m <- NULL
		
		## setting the functions for the parent environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		## get the matrix
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
		
		## Set the list for the values of makeCacheMatrix functions
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Uses values set in makeCacheMatrix to determine if matrix value needs to be calculated 
##if it hasn't calculate the inverse of the matrix that was stored in the "cache"

cacheSolve <- function(x, ...) {
        ## setting m variable as set in the 'x' environment
		m <- x$getmatrix()
		
		## check to see if the x environment has been evaluated before
		##if so return message and then return value of m 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		## if not calculated bring matrix into local variable data
        data <- x$get()
		
		## solve the inverse of the matrix 
        m <- solve(data, ...)
		
		## assign the calculated matrix to the 'x' environment using the setmatrix function
		##
        x$setmatrix(m)
        m
}
