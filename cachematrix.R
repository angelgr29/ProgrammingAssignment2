## Put comments here that give an overall description of what your
## functions do
#English is not my native language so please sorry for the mistakes I could make 
## Write a short comment describing this function

#makeCacheMatrix will create an R object that stores a matrix and its inverse 
# 

makeCacheMatrix <- function(x = matrix()) { #x as an empty matrix
     inv = NULL           #inv is null because its gonna be described later
     set= function(y){    #set is a function that depends on y (dummy variable)  
       x<<- y   # Assign the input argument to the x object in the parent environment, and
       inv<<- NULL #Inv also NULL in the parent environment
     }
   
      get= function() x#get the matrix x from the parent environment
    setinv= function(solve) {inv<<- solve} ##assign inv the solve function
    getinv= function() inv  #get the value of the inverse
    list(set= set, get=get, setinv= setinv,
         getinv=getinv)#retrieve the list to the parent environment
     
}


## Write a short comment describing this function

#cacheSolve is used to retrieve the inverse from an object of type makeMatrix

cacheSolve <- function(x, ...) {   #Remember that x is a matrix
       inv= x$getinv()   #get the inverse of x and assing it to inv
       if(!is.null(inv)){   #when inv is not null we are retrieving from cahed data
         message("getting cached data")
         return(inv)
       }
   data= x$get()###get the matrix
   inv= solve(data,...) ##compute the inverse and then print it 
   x$setinv(inv)
   inv
}


my_matrix= makeCacheMatrix(matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2))
my_matrix$get()
my_matrix$getinv()
cacheSolve(my_matrix)
my_matrix$getinv()
