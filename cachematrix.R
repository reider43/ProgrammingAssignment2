## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(workingMatrix = matrix()) {
        
        ## Clear the current Inverse Matrix
        inverseMatrix<-NULL 
        
        ## Create a Set funciton in the stored Names Matrix
        SetMatrix <-function(y){ 
                
                ## Set the Matrix to the passed in one
                workingMatrix<<-y 
                
                ## Reset the InverseMatrix is set is called
                inverseMatrix<<-NULL
        }
        
        ## Return the set Matrix
        GetMatrix<-function() workingMatrix 
        
        ## Set the Inverse for the Matrix
        SetInverse<-function(calculatedMatrix) inverseMatrix <<-calculatedMatrix
        
        ## Get the Inverse for the Matrix
        GetInverse<-function() inverseMatrix
        
        ## Setup the List
        list(SetMatrix=SetMatrix,GetMatrix=GetMatrix,
             SetInverse=SetInverse, GetInverse=GetInverse)

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already ## been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(workingMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Retrieve the Cached Inverse WorkingMatrix
        findInverse<-workingMatrix$GetInverse()
        
        ## Is there a cached value for the Inverse WorkingMatrix
        if(!is.null(findInverse)){
                
                ##Notify the user of the use of a cached Inverse Matrix
                message("Getting Cached Inverse Matrix")
                
                ##Return the cached value
                return(findInverse)
        }
        
        ##Get the values of the WorkingMatrix
        workingMatrixValues<-workingMatrix$GetMatrix()
        
        ## Find the Inverse of the WorkingMatrix
        findInverse<-solve(workingMatrixValues,...)
        
        ## Store the Inverse of WorkingMatrix for caching
        workingMatrix$SetInverse(findInverse)
        
        ## Print our the Inverse of the WorkingMatrix
        findInverse
        
}
