### makeCacheMatrix funciton gets the Matrix and returns a list of Functions 
### which is used in the cacheSolve function. cacheSolve returns the Inverse of the Matrix
### either from the cache (if available) or newly calculated 
makeCacheMatrix<-function(x=matrix())
{
	# function to get value of x
	getx<-function() x

#function to store the value of x in x1 - for comparing between current and previous values
	setx <-function()
	{
		x1<<-x
		}
# function to get value of x1		
getx1<-function()
	{
		x1
	}
#function to set the value of the Inverse of Matrix
setInv<-function(Inv1) 
{
Inv<<-Inv1
}

# function to get the value of the matrix
getInv<-function() Inv

list(getx=getx,getx1=getx1,setx=setx,setInv=setInv,getInv=getInv)
}

cacheSolve<-function(x, ...)
{
	
	
	mat<-x$getx1() ## get the previous input matrix 
	check<-x$getx() ## get teh current input matrix
	Inv<-x$getInv()
	if(is.matrix(mat)&&is.matrix(check)&&dim(mat)==dim(check)&&all(mat==check)) ## check if current and previous matrix are same
		{
    message("retruning from cache") # return from the cache if current and previous inputs are same
    Inv
		}
		else  {
	Inv1<-solve(check)  #derive the inverse of the matrix 
	x$setx()
	x$setInv(Inv1) 
	Inv1
	}
}