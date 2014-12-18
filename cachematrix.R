## makeCacheMatrix takes the original matrix and creates some functions that get called by
## cacheSolve. cacheSolve tries to retrieve the data from the solve(matrix) function. If it is there
## it prints it, if not, it calculates it, stores it, and prints it.

makeCacheMatrix <- function(x = matrix()) { ## The function gets passed a matrix, defaults to an empty matrix
        s <- NULL                           ## make sure s is NULL when this starts
        set <- function(y) {                ## set will store the value of the matrix but doesn't seem to get used.
                x <<- y
                s <<- NULL
        }
        get <- function() x                 ## get has an empty function then gets passed x which is the matrix
        setsolve <- function(solve) s <<- solve ## setsolve is a function that gets passed the value s which is "solve"
        getsolve <- function() s                ## getsolve is an empty function that passes s which is the cached solve value
        list(set = set, get = get,              ## list of function names is returned by makeCacheMatrix along with the matrix passed to it originally   
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve should get passed the value set from makeCacheMatrix. It
## will check the value of "s" from the above code and if it is empty, 
## calculate solve(matrix), return (and store) the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        s <- x$getsolve()                      ## s will receive the value of s set above. If s is NULL then the
        if(!is.null(s)) {                      ## solve has not been done for this matrix yet. The check is for non-null s
                message("getting cached data") ## values, which mean the solve() has been done and s contains the results
                return(s)                      ## so "s" is returned.
        }
        data <- x$get()                         ## this gets run only if s is NULL. It gets the original matrix value and puts it in 'data'
        s <- solve(data, ...)                   ## runs the solve() function on it and stores it in 's'
        x$setsolve(s)                           ## setsolve() from above gets passed s which stores it in a global environment
        s                                       ## and then returns the value of it. 
}






