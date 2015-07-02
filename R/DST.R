##' Combination rules
##' 
##' Different rules to combine masses
##' 
##' @export
##' 
##' @param MassIn The matrix containing the masses. Each column represents a
##' piece of mass.
##' @param criterion The combination criterion:
##' 
##' criterion=1 Smets criterion (conjunctive combination rule)
##' 
##' criterion=2 Dempster-Shafer criterion (normalized)
##' 
##' criterion=3 Yager criterion
##' 
##' criterion=4 Disjunctive combination criterion
##' 
##' criterion=5 Dubois criterion (normalized and disjunctive combination)
##' 
##' criterion=6 Dubois and Prade criterion (mixt combination), only for Bayesian masses whose focal elements are singletons
##' 
##' criterion=7 Florea criterion
##' 
##' criterion=8 PCR6
##' 
##' criterion=9 Cautious Denoeux Min for functions non-dogmatics
##' 
##' criterion=10 Cautious Denoeux Max for separable masses
##' 
##' criterion=11 Hard Denoeux for functions non-normales
##' 
##' criterion=12 Mean of the bbas
##' 
##' criterion=13 LNS rule, for separable masses
##' 
##' criterion=131 LNSa rule, for separable masses
##' @param TypeSSF If TypeSSF = 0, it is not a SSF, the general case. If TypeSSF = 1, a SSF with a singleton as a focal element. If TypeSSF = 2, a SSF with any subset of \eqn{\Theta} as a focal element. 
##' @return The combined mass vector. One column. 
##' @examples
##' 
##' m1=c(0,0.4, 0.1, 0.2, 0.2, 0, 0, 0.1);
##' m2=c(0,0.2, 0.3, 0.1, 0.1, 0, 0.2, 0.1);
##' m3=c(0.1,0.2, 0, 0.1, 0.1, 0.1, 0, 0.3);
##' 
##' m3d=discounting(m3,0.95);
##' 
##' M_comb_Smets=DST(cbind(m1,m2,m3d),1);
##' M_comb_Smets
##' M_comb_PCR6=DST(cbind(m1,m2),8);
##' M_comb_PCR6
##' M_comb_LNS = DST(cbind(m1,m2),13);
##' M_comb_LNS 
##' M_comb_LNSa = DST(cbind(m1,m2),131);
##' M_comb_LNSa 
##' 
##' n1 = 5
##' ThetaSize = 3
##' mass_mat = matrix(0, 2^ThetaSize, n1 + 1);
##' mass_mat[2, 1 : n1] = c(0.12, 0.16, 0.15, 0.11, 0.14) 
##' mass_mat[3, n1 + 1] = 0.95;
##' mass_mat[8, ] = 1 - colSums(mass_mat)
##' mass_ssf_mat = mass_mat[c(2^(1:ThetaSize-1)+1, 8), ]
##' # the following three functions could produce the same results
##' DST(mass_mat, 13)
##' DST(mass_mat, 13, TypeSSF = 2)
##' DST(mass_ssf_mat, 13, TypeSSF = 1)


DST <- function(MassIn, criterion, TypeSSF = 0){

	 case = criterion;
	 
     n = nrow(MassIn);
	 m = ncol(MassIn);

     if(case %in% c(4, 5, 6, 7)){
		 b = matrix(1,1,n);
		 for(i in 1:m){
			bj = mtob(MassIn[,i]);
			b = b*bj;
		 }
	 }
	 
	 if(case %in% c(1, 2, 3, 6, 7)){
		 q = matrix(1,1,n);
		 for(i in 1:m){
			qj = mtoq(MassIn[,i]);
			q = q*qj;
		 }
	 }


		if(case==1){
			#Smets criterion
			Mass=qtom(q);
			Mass[1]=1-sum(Mass[2:length(Mass)]); #In case with very high conflict Mass(1) could be >1 !
		}else if (case==2){
			#Dempster-Shafer criterion (normalized)
			Mass=qtom(q);
			Mass=Mass/(1-Mass[1]);                 
			Mass[1]=0;
		}else if (case==3){
			#Yager criterion
			Mass=qtom(q);
			Mass[n]=Mass[n]+Mass[1];
			Mass[1]=0; 
		}else if (case==4){
			# disjunctive combination criterion
			Mass=btom(b);
		}else if (case==5){
			#Dubois criterion (normalized and disjunctive combination)
			Mass=btom(b);
			Mass=Mass/(1-Mass[1]);
			Mass[1]=0;
		}else if (case==6){
			#Dubois and Prade criterion (mixt combination). Only if the focal
			#element are the singletons
			Mass=qtom(q);
			Mass[n]=0;
	   
			Mass_disjonc=btom(b);
			Mass_disjonc[n]=0;
			for(i in 1:floor(log2(n))){
				Mass_disjonc[1+2^(i-1)]=0;
     		}
			Mass=Mass+Mass_disjonc;
			Mass[1]=0;
			Mass[n]=1-sum(Mass);
		}else if (case==7){
			#Florea criterion
			Mass_conjonc=qtom(q);
			Mass_disjonc=btom(b);
			#alpha=Mass_conjonc(1)/(1-Mass_conjonc(1)+Mass_conjonc(1)*Mass_conjonc(1));
			#beta=(1-Mass_conjonc(1))/(1-Mass_conjonc(1)+Mass_conjonc(1)*Mass_conjonc(1));
			
			k=Mass_conjonc[1];
			x=0.9;
			alpha=log((1+x)/(k+x))/log((1+x)/x);
			beta=log((1+x)^k*(k+x)^(1-k)/x)/log((1+x)/x);
			
			Mass=alpha*Mass_disjonc+beta*Mass_conjonc;
		
			Mass[1]=0;
		}else if (case==8){
			#PCR6 combination Martin & Osswald Criteria
		     re=PCR6(MassIn);	
			 Conf=re$Conf;
			 Mass=re$Mass;
		 	 Mass=t(Mass);
			
		}else if (case==9){
			#Cautious Denoeux min for fonctions non-dogmatic
# 			 wtot=c();
# 			 w=c();
# 			 for(i in 1:m){
# 				wj=mtow(MassIn[,i]);
# 				wtot=cbind(wtot,t(wj));
# 			 }
			wtot = apply(MassIn, 2, mtow)
			#w=min(wtot,[],2);
			w=apply(wtot,1,min)
# 			w=t(w);
			Mass=wtom(w);
		}else if (case==10){
			#Cautious Denoeux max only for separable fonctions
# 			 wtot=c();
# 			 w=c();
# 			 for(i in 1:m){
# 				wj=mtow(MassIn[,i]);
# 				wtot=cbind(wtot,t(wj));
# 			 }
			wtot = apply(MassIn, 2, mtow)
			#w=max(wtot,[],2);
			w=apply(wtot,1,max)
# 			w=t(w);
			Mass=wtom(w);
		}else if (case==11){ 
			# Hard Denoeux for fonctions sous-normales
# 			 vtot=c();
# 			 v=c();
# 			 for(i in 1:m){
# 				vj=mtov(MassIn[,i]);
# 				vtot=cbind(vtot,t(vj));
# 			 }
			vtot = apply(MassIn, 2, mtov)
			#v=min(vtot,[],2);
			v=apply(vtot,1,min)
# 			v=t(v);
			Mass=vtom(v);
		}else if (case==12){
			# mean of the masses
			 Mass = apply(MassIn,1,mean);
		 
		}else if (case==13){
	        # LNS rule	
			if(TypeSSF == 0){
			  Mass = tCombine(MassIn, mygamma = 1)
			}else if(TypeSSF == 1){
			  Mass = tCombine_SSF(MassIn, mygamma = 1, singleton = TRUE)
			}else if(TypeSSF == 2){
			  Mass = tCombine_SSF(MassIn, mygamma = 1)
			}
		}else if (case == 131){
	        # LNSa rule	
			if(TypeSSF == 0){
			  Mass = tCombine(MassIn, approximate = TRUE)
			}else if(TypeSSF == 1){
			  Mass = tCombine_SSF(MassIn, mygamma = 1, approximate = TRUE, signleton = TRUE)
			}else if(TypeSSF == 2){
			  Mass = tCombine_SSF(MassIn, mygamma = 1, approximate = TRUE)
			}
		}else{
			stop('Accident: in DST choose of criterion: uncorrect\n');
        }
	return(matrix(Mass,,1))
}



