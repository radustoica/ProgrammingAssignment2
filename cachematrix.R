


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	inv = NULL;
	set = function(y) {
		x <<- y;
		inv <<- NULL;
	}
	get = function() { x; }

	setInv = function(inverse) { inv <<- inverse; }
	getInv = function() { inv; }

	list(set = set, get = get, setInv = setInv, getInv = getInv );
}


## cacheSolve function computes the invers of the special "matrix" returned by makeCacheMatrix above;
## if the inverse has already been calculated(and the matrix has not changed), the the cacheSolve
## should retrieve the inverse from the cache

cacheSolve <- function(x) {

	inv = x$getInv();
	if(!is.null(inv)) {
		message("getting cached data");
		return (inv);
	}

	data = x$get();
	inv = solve(data);
	x$setInv(inv);
	inv;

}









