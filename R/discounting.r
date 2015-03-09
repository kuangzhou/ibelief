##' Discounting masses
##'
##' Discount masses using  given factors 
##'
##' @export
##' @param MassIn Matrix with \eqn{2^n} rows and \eqn{nb} columns. Parameter \eqn{n} is the number of classes and \eqn{nb} is the number of experts.
##' @param alpha Discounting factor. A number or a vector with length of \code{ncol(MassIn)} 
##' @return Mass matrix. The discounted masses, each column is a piece of mass
##' @examples
##' ## The conflict table for two experts in a discernment frame with three elements
##' m1=c(0,0.4, 0.1, 0.2, 0.2, 0, 0, 0.1);
##' m2=c(0,0.2, 0.3, 0.1, 0.1, 0, 0.2, 0.1);
##' discounting(m1,0.95)
##' # if only one factor is given, all the masses are discounted using the same factor
##' discounting(cbind(m1,m2),0.95)
##' # if the factor vector is given, the masses are discounted using the corresponding factor
##' discounting(cbind(m1,m2),c(0.95,0.9))
discounting <- function(MassIn,alpha){

# MassIn: nb of masses = nb column
#         1 column=1 mass
    

	if(!is.matrix(MassIn)){
	  MassIn=as.matrix(MassIn)
	}
	# number of focal elements
    mm=nrow(MassIn);
	# number of masses
	nn=ncol(MassIn);

    if(length(alpha)==1){
	   alpha=rep(alpha,nn);
	}
    
    if(length(alpha)==nn){
		alpha_mat=t(matrix(alpha,nn,mm))
		Mass=alpha_mat*MassIn;
		Mass[mm,]=1-colSums(as.matrix(Mass[-mm,]))
		return(Mass)
	}else{
		stop('Accident: in discounting the size of alpha is uncorrect\n')
	}


}
