## This code contains two functions, makeCahceMatrix and cacheSolve.  Togther, 
## these functions will calculate the inverse of a matrix and then cache it for
## future use.

## makeCacheMatrix creates a list with the following functions: set_data to set 
## the value of the input matrix for which the inverse is to be calculated; 
## get_data to retrieve the value of the input matrix; set_inverse to set the
## value of the inverse matrix after it has been calculated (eg, to cache the
## inverse in the global environment so it can be used later); and get_inverse
## to retrieve the inverse matrix if had previously been calcualted and cached
## This function is called by passing in a matrix.  It returns a list that can
## be referred to as a "cache matrix" that contains the four functions described.

makeCacheMatrix <- function(input_matrix = matrix()) {
        inverse_matrix <- NULL 
        
        # first function to set the value of the input matrix for which the 
        # inverse is to be calculated 
        set_data <- function(new_matrix) { 
                inverse_matrix <<- NULL
        }
        
        # second function that retrives the value of the input matrix
        get_data <- function() { 
                input_matrix
        } 
        
        # third function that sets the value of the inverse matrix
        set_inverse <- function(inverse) { 
                inverse_matrix  <<- inverse 
        }
        
        # fourth function that retrieves the value of the inverse matrix 
        get_inverse <- function() { 
                inverse_matrix
        }
        
        # return the list containing the four functions above
        list(set_data = set_data, get_data = get_data,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## cacheSolve calculates the inverse of a matrix created using the 
## makeCacheMatrix function.  Before solving the inverse, the function first
## checks to see whether the inverse has already been calculated by using
## the get_inverse function that is part of the list of the makeCacheMatrix
## matrix.  If the inverse has already been calculated and cached in the 
## global environment, it returns the inverse from the cache rather than 
## calculating it.  If the inverse has not already been calculated, the function
## will solve for the inverse and then store the inverse in the cache using the
## set_inverse function that is part of the list of the makeCacheMatrix matrix.
## This function is called by passing in a matrix that was created using the
## makeCacheMatrix function.  It returns the inverse of the matrix.

cacheSolve <- function(cache_matrix, ...) {
        # check to see if the inverse of the cache matrix already exists
        inverse_matrix <- cache_matrix$get_inverse() 
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix) 
        }
        data <- cache_matrix$get_data() 
        
        # calculate the inverse matrix of the cache matrix
        inverse_matrix <- solve(data, ...) 
        
        # set the inverse matrix in the cache
        cache_matrix$set_inverse(inverse_matrix) 
        
        inverse_matrix
}
