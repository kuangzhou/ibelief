tCombine_SSF <- function(MassIn, mygamma, ifnormalize = FALSE, ifdiscount = TRUE, approximate = FALSE, eta = 0, singleton = FALSE){

	## Problem find: for conjunctive rule (and also for DS rule), when the number of masses if quite large, 
	##             more than one focal elements will have mass one
    
    ## LNS rule.  Also can be used for conjunctive rule, cautious rule and DS rule

	## only for SSF and non-dogmatic masses
	## singleton = TRUE, all the focal elements are singletons
	## singleton = FALSE, any kind of focal elements, including singletons certainly

	## useful for the EKNN and BeliefKNN, when singleton = TRUE

 
    ## MassIn: if singleton = TRUE, it is a matrix of (ThetaSize+1) * n. Each column is a bba. 
	##           The first ThetaSize rows are for masses on the singleton, the last row is for total ignorance, Theta
	##         if singleton = FALSE, it is a matrix of (2^ThetaSize) * n. Each column is a bba

	## we note that, when all the focal elements are singletons, we can also use singleton = FALSE to get the same results


    ## mygamma is the parameter of the family of conjunctive and disjunctive rules using triangular norms by Denoeux.
	## mygamma = 1, with ifnormalize = FALSE, smets conjunctive rule
	## mygamma = 1, with ifnormalize = TRUE, Dempster rule
	## mygamma = 0, cautious rule
	## mygamma between 0 and 1, the generalized case of the rules using triangular by Denoeux
	## mygamma = 1, ifnormalize = FALSE, ifdiscount = TRUE, LNS rule
	## approximate = TRUE, LNSa rule, the approximation method for LNS rule
	## eta, the parameter in LNS rule, control the specificity of the decision, only singleton = TRUE is useful

    if(singleton){
		ThetaSize = nrow(MassIn) - 1;
		nf = 2^ThetaSize
		n = ncol(MassIn)
# 	w_mat = apply(MassIn, 2, mtow)
		w_mat = MassIn[1:ThetaSize, ];
		w_mat = 1 - w_mat
		eta = 0
    }else{
		nf = nrow(MassIn);
		ThetaSize = log2(nf);
		w_mat = MassIn[1: (nf - 1), ]
		w_mat = 1 - w_mat
	}
# 	w_mat[which(w_mat == 0)] = 1
    if(approximate){

		  num_eff = apply(w_mat, 1, function(x){sum(abs(x - 1) > 1e-6)}) 
		  id_eff = which(num_eff > 0)
		  num_group_eff = num_eff[id_eff];
		  if(eta != 0){
			  beta_vec = rep(1, length(id_eff));
			  myc = sapply(1:nf -1, function(xx){
									sum(dec2bin(xx, ThetaSize))
							}) 
			  beta_vec = (ThetaSize/myc[id_eff])^eta
		      alpha_vec = beta_vec * num_group_eff / sum(beta_vec * num_group_eff)
		  }else{
		      alpha_vec =  num_group_eff / sum(num_group_eff)
		  }
		  w_eff =  1 - alpha_vec 
		  if(singleton){
		    w_vec = rep(1, ThetaSize)
		  }else{
		    w_vec = rep(1, nf - 1)
		  }
		  w_vec[id_eff] = w_eff 
	}else{ 
		if(mygamma == 1){
# 		  browser()
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
		  if(eta != 0){
			  beta_vec = rep(1, length(id_eff));
			  myc = sapply(1:nf -1, function(xx){
									sum(dec2bin(xx, ThetaSize))
							}) 
			  beta_vec = (ThetaSize/myc[id_eff])^eta
		      alpha_vec = beta_vec * num_group_eff / sum(beta_vec * num_group_eff)
		  }else{
		     alpha_vec =  num_group_eff / sum(num_group_eff)
		  }
		  w_eff =  1 - alpha_vec + alpha_vec * w_eff 
		  w_vec[id_eff] = w_eff 
		}
	}
	w_vec_complete = rep(1, nf);
    if(singleton){
		w_vec_complete[2^(1:ThetaSize-1) + 1] = w_vec
	}else{
		w_vec_complete[1: (nf - 1)] = w_vec
	}
# 	browser()
	if(min(w_vec_complete)>0){
	   out = wtom(w_vec_complete)
	}else{
	   id = which(w_vec_complete == 0)
       out = rep(0, nf) 	
	   out[id] = 1
	}
	if(ifnormalize && mygamma == 1){
	   ## dempster rule
	   out[1] = 0;
	   out = out/sum(out)
	}
	return(t(out))
}


