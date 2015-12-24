## First function create a list of functions that can be called to get the given matrix or its inverse (if this has already been calculated).
## Second function solve a given matrix or give a cached value for the same solved matrix (if already calculated)

### makeCacheMatrix create list of functions (set, get, setsolved, getsolved) for a given matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }#set a value to matrix x
    get <- function() x #fuctions that give back value  x
    setsolved <- function(solve) inv <<- solve #set value of inverted matrix (aka solve(x)). It doesn't calculate solve(x) on his own!
    getsolved <- function() inv #fuctions that give back value of inverted matrix  (aka solve(x))
    list(set = set, get = get,
         setsolved = setsolved,
         getsolved = getsolved) #create and gives as a result of the function the list of functions above
}

## #solve (give the inverse of) the given matrix. It takes a value cached if it has already been calculated instead of calculating it from scratch.

cacheSolve <- function(x, ...) {
    inv <- x$getsolved()#give name inv to getsolved (the inverted matrix) if this has been already given by user using the previous function, so that this is used to calculate the result of the whole function. if getsolved doesnt exist... 
    if(!is.null(inv)) { #...if getsolved doesnt exist it go ahead with solve(x) 
        message("getting cached data")#message to give in case it uses getsolved instead of playing the function on his own
        return(inv)
    }
    data <- x$get()#this is what the function does in case getsolved is empty. give name data to x.. 
    inv <- solve(data,...)#...and solve x from scratch
    x$setsolved(inv)
    inv
}

