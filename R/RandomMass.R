##' Generating masses
##' 
##' Different ways to generate masses
##' 
##' @export
##' 
##' @param nbFocalElement The number of focal elements
##' @param ThetaSize  The length of the discernment frame \eqn{\Theta}
##' @param nbMass  The number of masses to generate
##' @param Type Which kind of mass to generate:
##'
##'           Type=1 for focal elements can be everywhere
##' 
##'           Type=2 for focal elements can not be on the emptyset
##' 
##'           Type=3 for no dogmatic mass : one focal element is on \eqn{\Theta} (ignorance)
##' 
##'           Type=4 for no dogmatic mass : one focal element is on \eqn{\Theta} (ignorance) and no focal elements are on the emptyset
##' 
##'           Type=5 for all the focal elements are the singletons
##' 
##'           Type=6 for all the focal elements are the singletons and on \eqn{\Theta} (ignorance)
##' 
##'           Type=7 for all the focal elements are the singletons and on \eqn{\Theta} (ignorance), but not on all the singletons
##' 
##'           Type=8 On only one defined singleton, on \eqn{\Theta} (ignorance), and others
##' 
##'           Type=9 On one defined singleton, on other singletons and on \eqn{\Theta} (ignorance)
##' 
##'           Type=10 On one focal element contain a defined singleton, on other focal elements and on \eqn{\Theta} (ignorance)
##' 
##'           Type=11 On one focal element contain a defined singleton, on other focal elements (not emptyset) and on
##' 	           \eqn{\Theta} (ignorance)
##'
##'           Type=12 For constant bba with nested focal elements,  all of them contain a defined singleton. If parameter singleton is set to 0, the defined singleton can be any one of the element in the discernment framework.
##'
##'           Type=13 For non-dogmatic constant bba with nested focal elements,  all of them contain a defined singleton. Different from Type 12, the mass given to \eqn{\Theta} must be positive (non-dogmatic). If parameter singleton is set to 0, the defined sigleton can be any one of the element in the discernment framework.
##'
##'           Type=14 For non-dogmatic constant bba with nested focal elements,  all of them contain a defined singleton. The focal elements must contain the defined sigleton and \eqn{\Theta}. If parameter singleton is set to 0, the defined sigleton can be any one of the element in the discernment framework. 
##' @param singleton The singleton element (with only one element) in the focal sets. It should be given a number from 1 to \eqn{ThetaSize} if Type is from 5 to 11.  
##' @return The generated mass matrix. Each column represents a piece of mass
##' @examples
##' 
##' RandomMass(nbFocalElement=3, ThetaSize=3, nbMass=4, Type=1)
##' RandomMass(nbFocalElement=3, ThetaSize=4, nbMass=4, Type=3)
##' RandomMass(nbFocalElement=4, ThetaSize=4, nbMass=4, Type=5,singleton=2)
##' RandomMass(nbFocalElement=4, ThetaSize=4, nbMass=4, Type=10,singleton=2)
##' RandomMass(nbFocalElement=4, ThetaSize=4, nbMass=4, Type=13,singleton=2)
##' RandomMass(nbFocalElement=2, ThetaSize=4, nbMass=4, Type=14,singleton=2)
##' 
RandomMass <- function(nbFocalElement, ThetaSize, nbMass, Type, singleton) {

    # depending program

    # new Sample function. To avoid the problem by the function base::sample 
	# if there is only one element, i, for Sample, base::sample will think that the data set to Sample is 1:i
	# for example, base::sample(5, 1) will return a random integer from 1 to 5
	# this may bring some problem when using base::sample(setdiff(..), ..), when the results of setdiff(..) is only one element

    
    indice <- function(ThetaSize, nbFocalElement, Type, singleton) {
        
        Type = Type
        # the empty set is 1; the Theta is 2^K
        nb = 2^ThetaSize
        if (Type == 1) {
            # focal elements can be everywhere
            ind = Sample(1:nb)
        } else if (Type == 2) {
            # focal elements can not be on the emptyset
            ind = Sample(2:nb)
        } else if (Type == 3) {
            # no dogmatic mass : one focal element is on Theta (ignorance)
            ind = c(nb, Sample(1:(nb - 1)))
        } else if (Type == 4) {
            # no dogmatic mass : one focal element is on Theta (ignorance) and no focal elements are on the emptyset
            ind = c(nb, Sample(2:(nb - 1)))
        } else if (Type == 5) {
            # all the focal elements are the singletons
            if (nbFocalElement == ThetaSize) {
                ind = 1 + 2^(1:ThetaSize - 1)
            } else {
                stop("Accident: in RandomMass - indice: nbFocalElement and ThetaSize are not the same\n")
            }
        } else if (Type == 6) {
            # all the focal elements are the singletons and on Theta (ignorance)
            if (nbFocalElement == ThetaSize + 1) {
                ind = c(1 + 2^(1:ThetaSize - 1), nb)
            } else {
                stop("Accident: in RandomMass - indice: nbFocalElement and ThetaSize+1 are not the same\n")
            }
        } else if (Type == 7) {
            # all the focal elements are the singletons and on Theta (ignorance), but not on all the singletons
            indtmp = Sample(1:ThetaSize)
            ind = 1 + 2^(indtmp[1:(nbFocalElement - 1)] - 1)
            ind = c(ind, nb)
        } else if (Type == 8) {
            # On only one defined singleton, on Theta (ignorance), and others
            # check the number of input arguments
            if (!missing(singleton)) {
                indRest = Sample(setdiff(1:nb, c(nb, 1 + 2^(singleton - 1))), nbFocalElement - 2)
                ind = c(nb, 1 + 2^(singleton - 1), indRest)
            } else {
                stop("Accident: in RandomMass - indice: the defined singleton is not given\n")
            }
        } else if (Type == 9) {
            # On one defined singleton, on other singletons and on Theta (ignorance)
			# check the number of input arguments
            if (!missing(singleton)) {
                indRest = Sample(setdiff(1 + 2^(1:ThetaSize - 1), 1 + 2^(singleton - 1)), nbFocalElement - 2)
                ind = c(1 + 2^(singleton - 1), indRest, nb)
            } else {
                stop("Accident: in RandomMass - indice: the defined singleton is not given\n")
            }
        } else if (Type == 10) {
            # On one focal element contain a defined singleton, on other focal elements and on Theta (ignorance) check the number of input
            # arguments
            if (!missing(singleton)) {
                myF = t(sapply(1:nb, function(x) {
                  dec2bin(x - 1, ThetaSize)
                }))
                indSing = which(myF[, singleton] == 1)
                Alea = Sample(setdiff(indSing, nb), 1)
                indRest = Sample(setdiff(1:(nb - 1), Alea), nbFocalElement - 2)
                ind = c(nb, Alea, indRest)
            } else {
                stop("Accident: in RandomMass - indice: the defined singleton is not given\n")
            }
        } else if (Type == 11) {
            # idem 10 without emptyset (On one focal element contain a defined singleton, on other focal elements and on Theta (ignorance)) check
            # the number of input arguments
            if (!missing(singleton)) {
                if (nbFocalElement != 2^ThetaSize) {
                  myF = t(sapply(1:nb, function(x) {
                    dec2bin(x - 1, ThetaSize)
                  }))
                  indSing = which(myF[, singleton] == 1)
                  Alea = Sample(setdiff(indSing, nb), 1)
                  indRest = Sample(setdiff(2:(nb - 1), Alea), nbFocalElement - 2)
                  ind = c(nb, Alea, indRest)
                } else {
                  stop("Accident: in RandomMass - indice: The number of focal element must be < 2^ThetaSize\n")
                }
            } else {
                stop("Accident: in RandomMass - indice: the defined singleton is not given\n")
            }
        } 
# 		else {
#             # otherwise
#             stop("Accident: in RandomMass - indice: choose of Type: incorrect\n")
#         }
        
        
        return(ind)
    }
    
    if (nbFocalElement < 2^ThetaSize + 1) {
        
		if (Type %in% 1:11){
            MassOut = matrix(0, 2^ThetaSize, nbMass)
			for (i in 1:nbMass) {
				ind = indice(ThetaSize, nbFocalElement, Type, singleton)
				indFocalElement = ind[1:nbFocalElement]
				randMass = diff(c(0, sort(runif(nbFocalElement - 1)), 1))
				MassOut[indFocalElement, i] = randMass
			}
		}else if (Type == 12){
            MassOut = RandomMass1(nbFocalElement, ThetaSize, nbMass, singleton, nondogmatic = FALSE, con_sig = FALSE)
		}else if (Type == 13){
            MassOut = RandomMass1(nbFocalElement, ThetaSize, nbMass, singleton, nondogmatic = TRUE, con_sig = FALSE)
		}else if (Type == 14){
            MassOut = RandomMass1(nbFocalElement, ThetaSize, nbMass, singleton, nondogmatic = TRUE, con_sig = TRUE)
		}else {
            stop("Accident: in RandomMass - indice: choose of Type: incorrect\n")
		}
    } else {
        stop("Accident: in RandomMass nbFocalElement > 2^ThetaSize\n")
    }
    return(MassOut)
} 

Sample <- function(x, size, replace = FALSE, prob = NULL) {
        if (missing(size)) 
            size <- length(x)
        x[sample.int(length(x), size, replace, prob)]
    }
