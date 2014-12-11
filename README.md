# Caching the Inverse of a Matrix

Below is a pair of functions create what could 
be called a matrix and inverse object.  

- To use, pass a matrix to the makeCacheMatrix() constructor.
  - The constructor takes a matrix (mat=) as its first and only argument
- The matrix can be retrieved with the accessor $get()
- To change the matrix, call the $set() mutator function.
- Calculate the inverse with cacheSolve() passing the the object created above
- Alternatively, call the $setinv() mutator function. 
  - Can be used in place of cacheSolve( matrix, ...).
- It's inverse can be retrieved with the accessor $getinv() once its calculated
- Once set, the inverse is cached for further use.
- The cached inverse is kept until the matrix is changed.

For example:
```
 > object <- makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2, 2)) # Creates object
 > object$get()
     [,1] [,2]
 [1,]   -1    1
 [2,]   -2    1
 > cacheSolve( object )
     [,1] [,2]
 [1,]    1   -1
 [2,]    2   -1
 > object$getinv()
     [,1] [,2]
 [1,]    1   -1
 [2,]    2   -1
```


## makeCacheMatrix()
Returns a list containing 4 functions with access to two data members.  
### The Functions are:
- get()    -- returns matrix
- set()    -- sets/changes matrix
- getinv() -- returns inverse if cached, else NULL
- setinv() -- sets/changes inverse

### Private Data Members are:
- mat      -- stores matrix
- inv      -- stores inverse if cached, else NULL

## cacheSolve(mat, ... )
Returns the cached inverse of mat if it exists.

Otherwise it caches and returns the result of solve(mat, ...)

The argument "mat" is assumed to be a list returned from makeCacheMatrix()
  and to contain an invertable matrix.  
  ***No validation of these assumptions is made.***
