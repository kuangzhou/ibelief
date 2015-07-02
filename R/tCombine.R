tCombine <- function(MassIn, mygamma = 1, ifnormalize = FALSE, ifdiscount = TRUE, eta = 0, approximate = FALSE){
    
    ## LNS rule.  Also can be used for conjunctive rule, cautious rule and DS rule
	## only for seperable mass
	## if want to use LNS rule, run like: tCombine(MassIn, mygamma = 1)
    ## if want to use LNSa rule, run like: tCombine(MassIn, approximate = TRUE)
 
	
    ## mygamma is the parameter of the family of conjunctive and disjunctive rules using triangular norms by Denoeux.
	## mygamma = 1, with ifnormalize = FALSE, smets conjunctive rule
	## mygamma = 1, with ifnormalize = TRUE, Dempster rule
	## mygamma = 0, cautious rule
	## mygamma between 0 and 1, the generalized case of the rules using triangular by Denoeux
	## mygamma = 1, ifnormalize = FALSE, ifdiscount = TRUE, LNS rule
	## approximate = TRUE, LNSa rule, the approximation method for LNS rule
	## eta, the parameter in LNS rule, control the specificity of the decision

    nf = nrow(MassIn)
	n = ncol(MassIn)
	ThetaSize = log2(nf)
	w_mat = apply(MassIn, 2, mtow)
    if(approximate){

		  num_eff = apply(w_mat, 1, function(x){sum(abs(x - 1) > 1e-6)}) 
		  id_eff = which(num_eff > 0)
		  num_group_eff = num_eff[id_eff];
		  beta_vec = rep(1, length(id_eff));
		  if(eta != 0){
   	   		myc = sapply(1:nf -1, function(xx){
								sum(dec2bin(xx, ThetaSize))
						}) 
			 beta_vec = (ThetaSize/myc[id_eff])^eta
		  }
		  alpha_vec = beta_vec * num_group_eff / sum(beta_vec * num_group_eff)
		  w_eff =  1 - alpha_vec 
		  w_vec = rep(1, nf)
		  w_vec[id_eff] = w_eff 
        
	}else{ 
		if(mygamma == 1){
		  ## conjunctive rule or dempster rule
		  w_vec = apply(w_mat, 1, prod)	
		}else if(mygamma == 0){
		  w_vec = apply(w_mat, 1, min) 
		}else{
		  ## I donot know if it is right for more than 2 masses
		  w_vec = apply(w_mat, 1, function(x){prod(x)/prod(max(c(x, mygamma))[1:(length(x)-1)])})	
		}

		if(ifdiscount){
		  ## find the masses that are not total ignorance
		  num_eff = apply(w_mat, 1, function(x){sum(abs(x - 1) > 1e-6)}) 
		  id_eff = which(num_eff > 0)
		  w_eff = w_vec[id_eff];
		  num_group_eff = num_eff[id_eff];
		  beta_vec = rep(1, length(id_eff));
		  if(eta != 0){
   			myc = sapply(1:nf -1, function(xx){
								sum(dec2bin(xx, ThetaSize))
						}) 
			 beta_vec = (ThetaSize/myc[id_eff])^eta
		  }
		  alpha_vec = beta_vec * num_group_eff / sum(beta_vec * num_group_eff)
		  w_eff =  1 - alpha_vec + alpha_vec * w_eff 
		  w_vec[id_eff] = w_eff 
		}
	}

    out = wtom(w_vec)
	if(ifnormalize && mygamma == 1){
	   ## dempster rule
	   out[1] = 0;
	   out = out/sum(out)
	}
	return(t(out))
}


# dec2bin  <-  function(nb, n) {
#     v = nb
#     if (v == 0) {
#         pos = 1
#     } else {
#         pos = floor(log(v, 2)) + 1
#     }
#     
#     if (!missing(n)) {
#         if (nb >= 2^n) {
#             warning("n is too small \n")
#         } else {
#             pos = n
#         }
#     }
#     bin = rep(0, pos)
#     i = 1
#     while (v != 0) {
#         p = v%/%2
#         if (2 * p == v) {
#             bin[i] = 0
#         } else {
#             bin[i] = 1
#         }
#         v = p
#         i = i + 1
#     }
#     return(bin)  #return binary encoding
# } 
# 
