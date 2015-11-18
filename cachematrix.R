## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.
## makeCacheMatrix: return a list of functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
 

        m <- NULL
		
		## almacena la matriz en el nuevo entorno
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		## obtiene la matriz del nuevo entorno
        get <- function() x
		
		## almacena la inversa
        setsolve <- function(solve) m <<- solve
		## obtiene la inversa
        getsolve <- function() m
		
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
 
# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
 cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
		
		## si la matriz inversa ya ha sido calculada, la obtiene
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		
		## en caso contrario, obtiene la matriz inversa
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}