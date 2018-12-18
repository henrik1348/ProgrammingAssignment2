
## The makeCacheMatrix function ultimately returns a list of the inverse. 

makeCacheMatrix <- function(x = matrix()) { #Default of argument x is an empty matrix.
    inv = NULL                              #Creates inv, an empty object to be used later.    
    set = function(y) {                     # function with argument y 
        x <<-y                              # y is assigned to x, the matrix in parent eniviroment of makeCacheMatrix. 
        inv <<- NULL                        # NULL is assigned to inv, also in parent enviroment.
    }
    get = function() x                      # x is not defined within function get(), therefore R retrive it from parent enivorment.
    setinver = function(inverse)            # setter function for the inverse.
        inv <<- inverse                     # inv is in parent enivorment.
    getinver = function() inv               # R retrive the value of inv from parent enivorment.
    list(set = set, 
         get = get, 
         setinver = setinver, 
         getinver = getinver)               # Finally, a list is returned.
}

## cacheSolve computes the inverse of the list that is returned from makeCacheMAtrix.
## If the inverse has been computed already, then this function will get the inverse from the cache. 
## This speed things up.


cacheSolve <- function(x, ...) {           # argument x refers to the list makeCacheMatrix returns.
    inv = x$getinv()                       # gets the inverse from x. 
    
    if (!is.null(inv)){                    # If the result is NULL..
        message("getting cached data")
        return(inv)                        # ...then return the value inv to parent enviroment.
    }
    
    data = x$get()                         # If !is.null is FALSE, the function get x.  
    inv = solve(data, ...)                 # And calucalte the inverse.
    x$setinv(inv)                          #     
    return(inv)                            # Returns the inverse.
}


# Test

myMatrix_object <- makeCacheMatrix(m1)

cacheSolve(myMatrix_object)

#       [,1] [,2]
# [1,]    6    8
# [2,]    2    4
