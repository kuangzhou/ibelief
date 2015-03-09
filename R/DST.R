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
##' criterion=1 Smets criterion
##' 
##' criterion=2 Dempster-Shafer criterion (normalized)
##' 
##' criterion=3 Yager criterion
##' 
##' criterion=4 Disjunctive combination criterion
##' 
##' criterion=5 Dubois criterion (normalized and disjunctive combination)
##' 
##' criterion=6 Dubois and Prade criterion (mixt combination)
##' 
##' criterion=7 Florea criterion
##' 
##' criterion=8 PCR6
##' 
##' criterion=9 Cautious Denoeux Min for functions non-dogmatics
##' 
##' criterion=10 Cautious Denoeux Max for separable functions
##' 
##' criterion=11 Hard Denoeux for functions non-normales
##' 
##' criterion=12 Mean of the bbas
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
##' 
DST <- function(MassIn,criterion){

	 n=nrow(MassIn);
	 m=ncol(MassIn);
	 b=matrix(1,1,n);
	 q=matrix(1,1,n);
	 for(i in 1:m){
		bj=mtob(MassIn[,i]);
		qj=mtoq(MassIn[,i]);
	   
		b=b*bj;
		q=q*qj;
	 }

	case=criterion;

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
			 wtot=c();
			 w=c();
			 for(i in 1:m){
				wj=mtow(MassIn[,i]);
				wtot=cbind(wtot,t(wj));
			 }
			#w=min(wtot,[],2);
			w=apply(wtot,1,min)
			w=t(w);
			Mass=wtom(w);
		}else if (case==10){
			#Cautious Denoeux max only for separable fonctions
			 wtot=c();
			 w=c();
			 for(i in 1:m){
				wj=mtow(MassIn[,i]);
				wtot=cbind(wtot,t(wj));
			 }
			#w=max(wtot,[],2);
			w=apply(wtot,1,max)
			w=t(w);
			Mass=wtom(w);
		}else if (case==11){ 
			# Hard Denoeux for fonctions sous-normales
			 vtot=c();
			 v=c();
			 for(i in 1:m){
				vj=mtov(MassIn[,i]);
				vtot=cbind(vtot,t(vj));
			 }
			#v=min(vtot,[],2);
			v=apply(vtot,1,min)
			v=t(v);
			Mass=vtom(v);
		}else if (case==12){
			# mean of the masses
			 Mass=apply(t(MassIn),2,mean);
		 
		}else{
			stop('Accident: in DST choose of criterion: uncorrect\n');
        }
	return(t(Mass))
}



