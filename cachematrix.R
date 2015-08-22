##This are functions that will allow the cache of the inverse of a matrix, KMG

makeCacheMatrix <- function(x = matrix()) {
        ## @x: a square invertible matrix
        ## a list of the four functions, set and get matrix, and inverse.
        ##         this are input used for function cacheSolve()
        
        inver = NULL # this gives a defualt value where cachesolve() 
        # has not been used
        set = function(y) {
                # I have used `<<-`  operator to assign values to objects
                # in different environments.
                x <<- y
                inver <<- NULL
        }
        get = function() x
        
        setinver = function(inverse) inver <<- inverse 
        
        getinver = function() inver
        
        list(setmat=setmat, getmat=getmat, setinv=setinver, getinv=getinver)
}



cacheSolve <- function(x, ...) {
        ## This function will find the inverse of the 
        ## original input of makeCacheMatrix()
        
        inver = x$getinver()
        
        
        if (!is.null(inver)){ # if we already have the inverse 
                # as it has been calculated
                
                message("getting cached data, so please wait")  # takes from cache, doesn't compute
                # shows display message
                return(inver) # returns cached value
        }
        
        # if not then just do the computation
        matdat = x$get()
        inver = solve(matdat, ...)
        
        # sets value of the cache inverse from the setinver function.
        x$setinver(inver)
        
        return(inver)
}