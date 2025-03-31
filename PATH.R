PATH=function(data,p,t,tun,L,R,lam){
  
  AA=rep(0,t)
  BB=matrix(0,p,t)
  GG=matrix(0,p,t)
  
  for (j in 1:t) {
    
    sub=data[which(data[,p+4]==j),]
    #pathway lasso
    if(tun==T){ #tuning parameter selection using stability criterion
      fit=mediate_plasso(M=sub[,1:p],A=sub[,p+1],Y=sub[,p+2],lambdas=seq(L,R,length.out=5),select_lambda=T)
      BB[,j]=fit$chosen_fit$beta
      GG[,j]=fit$chosen_fit$alpha
      AA[j]=fit$chosen_fit$direct_effect[1]
    }else{
      fit=mediate_plasso(M=sub[,1:p],A=sub[,p+1],Y=sub[,p+2],lambdas=lam)
      BB[,j]=fit$all_fits$lambda1$beta
      GG[,j]=fit$all_fits$lambda1$alpha
      AA[j]=fit$all_fits$lambda1$direct_effect[1]
    }
  }
  return(round(c(rbind(AA,BB,GG)),2))
}