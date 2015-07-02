RandomSSF <- function(ThetaSize, nbMass, Include){
    # Include is the focal element of SSF except Theta 
	nf = 2^ThetaSize
	if(missing(Include)){
	   Include = sample(2:(nf-1), nbMass, replace = TRUE)
	}
	if(length(Include) == 1){
	   Include = rep(Include, nbMass)
	}
	MassOut = matrix(0, nf, nbMass);
	for(i in 1:nbMass){
	   MassOut[c(Include[i], nf) ,i] =  runif(2);
	}
    MassOut =  MassOut / (matrix(1, nf, 1) %*% colSums(MassOut)) 
	return(MassOut)
}
