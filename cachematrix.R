## Put comments here that give an overall description of what your
## functions do
## These functions cache the inverse of a matrix.  I've got no earthly idea what
## a "square invertable matrix" is, but the function "solve"
## seems to take care of it.

## Write a short comment describing this function 
## Basically just replaces all cosmetic instances of "Mean" in example code with
## "Matrix," and functional instances with "solve."
## Seems to work.
## Sets up functions to deal with a matrix and cache results.  Set up "M" as a 
## null value in the global environment.  Sets up "x" globally as a list of 
## functions.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get <-function() x
        setmatrix <-function(solve) m <<-solve
        getmatrix <-function() m
        
        list(set = set, get= get, 
             setmatrix = setmatrix, 
             getmatrix = getmatrix
        )
}


## Write a short comment describing this function
## Checks whether or not M is null.  If it's not, then it just returns what's 
## there (and the message 'getting cached data'.)  If it is, it uses the global
## "x" list run functions on the matrix (stored in "get.")


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <-x$get()
        m<-solve(data, ...)
        x$setmatrix(m)
        m
}
