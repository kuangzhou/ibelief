RandomMass1 <- function(nbFocalElement, ThetaSize, nbMass, singleton, nondogmatic = TRUE, con_sig = FALSE) {

    ## generated nested bba
	## the parameters are the same as that in RandomMass in ibelief Package
	## singleton = 0, random singleton, could be any one in the discernment framework
	## con_sig, if contains the singleton element as a focal element
	## For SSF with only singleton and Theta, run like: RandomMass(2, ThetaSize, nbMass, singleton, TRUE, TRUE)
    
#     Sample <- function(x, size, replace = FALSE, prob = NULL) {
#         if (missing(size)) 
#             size <- length(x)
#         x[sample.int(length(x), size, replace, prob)]
#     }

    indice <- function(ThetaSize, singleton) {
        nb = 2^ThetaSize
        ind = rep(0, ThetaSize)
		if(singleton == 0){
		  singleton = Sample(1:ThetaSize, 1) 
		}
        ind[1] = 2^(singleton - 1) + 1
        temp = rep(0, ThetaSize)
        temp[singleton] = 1
        for (i in 2:ThetaSize) {
            id_cond = which(temp == 0)
			id_push = Sample(id_cond, 1)
            # cat(id_push, '\n')
            temp[id_push] = 1
            ind[i] = bin2dec(temp) + 1
        }
        
        # cat(ind, '\n')
        return(ind)
    }
    
    
    if (nbFocalElement <= ThetaSize) {
        MassOut = matrix(0, 2^ThetaSize, nbMass)
        for (i in 1:nbMass) {
            ind = indice(ThetaSize = ThetaSize, singleton = singleton)
#             cat("here-")
#             cat(ind, "\n")
			if(nondogmatic){
			  if(con_sig){
               indFocalElement = c(Sample(ind[2: (ThetaSize - 1)], nbFocalElement - 2), ind[1], 2^ThetaSize)
			  }else{
               indFocalElement = c(Sample(ind[1: (ThetaSize - 1)], nbFocalElement - 1), 2^ThetaSize)
			  }
			}else{
               indFocalElement = Sample(ind, nbFocalElement)
			}
			# cat(indFocalElement, '\n')
            randMass = diff(c(0, sort(runif(nbFocalElement - 1)), 1))
            MassOut[indFocalElement, i] = randMass
        }
    } else {
        stop("Accident:  in RandomMass for nested bbas, nbFocalElement > ThetaSize\n")
    }
    
    return(MassOut)
}


bin2dec <- function(x) {
    return(sum(2^(1:length(x) - 1) * x))
} 
