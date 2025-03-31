BAY=function(data,p,t){
  
  AA=rep(0,t)
  BB=matrix(0,p,t)
  GG=matrix(0,p,t)
  
  for (j in 1:t) {
    
    sub=data[which(data[,p+4]==j),]
    #Bayesian mediation analysis
    fit=bama(M=sub[,1:p],A=sub[,p+1],Y=sub[,p+2],method="BSLMM",
             burnin=1000,ndraws=2000,C1=matrix(1,nrow(sub),1),C2=matrix(1,nrow(sub),1))
    
    len1=which(colMeans(fit$r1)>=0.5)
    len2=which(colMeans(fit$r3)>=0.5)
    
    BB[len1,j]=colMeans(fit$beta.m)[len1]
    GG[len2,j]=colMeans(fit$alpha.a)[len2]
    
    AA[j]=mean(fit$beta.a)
  }
  return(round(c(rbind(AA,BB,GG)),2))
}