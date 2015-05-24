# ----------------------------------------------------------------------------------------
#     makeCacheMatrix.R - Written by Nick Kemp - May 2015
# Used to cache the inverese of a matrix 
# When a new matrix is set then the cached inverse of the old matrix is deleted
# ----------------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
    
    myCache <- NULL                     # initialise the cache
    
    set <- function(y) {                # when a new matrix is in play
        currentMatrix <<- y             # set currentMatrix to be the matrix
        myCache <<- NULL                # set the cache to null
    }
    
    get <- function(){                  # returns what ever the current matrix is
        return(currentMatrix)        
    }
    
    setInv <- function(matInv){         # sets the cache to a new inverted matrix 
        myCache <<- matInv
    }  
    
    
    getInv <- function(){               # Returns the current cached inverted matrix
        return(myCache)
    }
    
                                        # This allows the internal functions to be called
    list(set = set,  
         get = get,
         setInv = setInv,
         getInv = getInv)
}



# ----------------------------------------------------------------------------------------
#     cacheSolve.R - Written by Nick Kemp - May 2015
# Returns the inverse of a matrix either fron the cache is it exists or freshly calculated
# ----------------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
    m <- x$getInv()                     # Attempt Collecting Inverted matrix from makeCacheMatrix
    
    if(is.null(m)) {                    # If it doesn't exist 
        data <- x$get()                 # then get the current matrix from makeCacheMatrix
        m <- solve(data, ...)           # calculate the inverse using solve() 
        x$setInv(m)                     # and store the result in makeCacheMatrix
    }
    else{        
        message("getting cached data")  
        
    }
    
    return(m)                           # Return the inverted matrix
}


