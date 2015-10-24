# makeCacheMatrix creates a special matrix that can cache its inverse.  
# This really is a list containing a function to:
#
#    1. Set the value of the matrix
#    2. Get the value of the matrix
#    3. Get the value of the inverse
#    4. Get the value of the inverse
#
# This list is used as the input to the cacheSolve() function.
# 
# Note: The "super assigment" operators <<- and ->> are normally only used in 
# functions, and cause a search to made through parent environments 
# for an existing definition of the variable being assigned. 
# If such a variable is found (and its binding is not locked) then its value is 
# redefined, otherwise assignment takes place in the global environment. 
# Note that their semantics differ from that in the S language, but are useful 
# in conjunction with the scoping rules of R. 
# See 'The R Language Definition' manual for further details and examples. 
# References:
# http://stat.ethz.ch/R-manual/R-patched/library/base/html/assignOps.html

makeCacheMatrix <- function(x = matrix()) {
        # Creates cached matrix.
        #
        # Args:
        #      x: Square matrix to be cached (should be invertible)
        #    	
        # Returns:
        #    Cached matrix to be used in the cacheSolve() function
        #
        
        # initialize to null values
        inv <- NULL						
        
        # create the matrix in the working environment (global environment).
        # use the "super assignment" operator (<<-) to assign a value 
        # to an object that is different from the current environment.
        # (This enables us to modify a variable declared outside of the
        # current function in which the reference to the variable is made).
        set <- function(y) {				
                x <<- y					
                inv <<- NULL
        }       						
        
        # get the value of the matrix
        get <- function() x					
        # invert the matrix and store in the cache
        setinv <- function(inverse) inv <<- inverse		 
        # get the inverted matrix from the cache
        getinv <- function() inv				
        
        # return the created functions to the working environment
        list(set=set, get=get, setinv = setinv, getinv = getinv)
        
}

# The cacheSolve() function computes the inverse of the matrix returned by the 
# makeCacheMatrix() function.  However, it first checks to see if the inverse 
# has already been calculated and the matrix has not changed.
# If so, it gets the inverse directly from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the 
# inverse in the cache via the setinv function.
# Note: One assumption here is the supplied matrix is always invertible.

cacheSolve <- function(x, ...) {      
        # Computes the inverse of the matrix returned by makeCacheMatrix().
        # Attempts to retrieve the value from cache if possible.
        #
        # Args:
        #      x: Cached matrix object
        #    ...: Optional parameters to the solve function	
        #
        # Returns:
        #    The inversion of a given matrix.
        #
        
        # return a matrix that is the inverse of x
        inv <- x$getinv()				
        
        # if cache is not null (i.e., cache exists),
        # then retrieve it from the cache and skip the computation
        if(!is.null(inv)){  				
                message("getting cached data")		
                return(inv)
        }
        
        # otherwise, calculate the inverse in the working environment.
        # create the matrix in working environment
        mtrx.data <- x$get()				
        # use solve function to set and return the inverse of a square matrix 
        inv <- solve(mtrx.data, ...)			
        
        
        # set the value of the inverse in the cache using the setinv function
        x$setinv(inv)
        
        return(inv)					
}

