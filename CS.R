CS=function(data,p,t,lam1,a=3.7,c=10){
  
  N=nrow(data)
  ##################################################################################
  #SCAD penalty
  scad=function(u,lam=lam1){
    lam*u*(u<=lam)+
      (a*lam*u-0.5*(u^2+lam^2))/(a-1)*((lam<u)&(u<=a*lam))+
      0.5*(a+1)*lam^2*(u>a*lam)
  }
  #derivative
  scadd=function(u,lam=lam1){
    lam*((u<=lam)+(a*lam-u)*(a*lam>u)/(a-1)/lam*(u>lam))
  }
  #sum of squares of the residuals
  rss=function(data,theta){
    
    fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
    AA=fen[1,]
    BB=fen[2:(1+p),]
    GG=fen[(p+2):(1+2*p),]
    DD=theta[(1+2*p)*t+1]
    TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
    ##############################################################
    r1=0
    r2=0
    for (j in 1:t) {
      
      sub=data[which(data[,p+4]==j),]
      
      e1=sub[,p+2]-cbind(sub[,c(p+1,1:p)],1)%*%c(AA[j],BB[,j],DD)
      e2=sub[,1:p]-cbind(sub[,p+1],1)%*%rbind(GG[,j],TT)
      
      r1=r1+sum(e1^2)
      r2=r2+sum(e2^2)
    }
    return(c(r1,r2))
  }
  #derivative
  dev=function(data,theta){
    
    fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
    AA=fen[1,]
    BB=fen[2:(1+p),]
    GG=fen[(p+2):(1+2*p),]
    DD=theta[(1+2*p)*t+1]
    TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
    ##############################################################
    ph1=rep(0,(1+2*p)*t)
    ph2=rep(0,1+p)
    for (j in 1:t) {
      
      sub=data[which(data[,p+4]==j),]
      
      e1=sub[,p+2]-cbind(sub[,c(p+1,1:p)],1)%*%c(AA[j],BB[,j],DD)
      e2=sub[,1:p]-cbind(sub[,p+1],1)%*%rbind(GG[,j],TT)
      
      len=((j-1)*(1+2*p)+1):(j*(1+2*p))
      ph1[len]=c(c(e1)%*%sub[,c(p+1,1:p)],sub[,p+1]%*%e2)
      ph2=ph2+c(sum(e1),colSums(e2))
    }
    g1=-2*c(ph1,ph2)
    return(g1)
  }
  ###############################################################################################################
  AA=rep(0,t)
  BB=matrix(0,p,t)
  GG=matrix(0,p,t)
  DD=0
  TT=rep(0,p)
  
  for (j in 1:t) {
    
    sub=data[which(data[,p+4]==j),]
    
    fit=cv.glmnet(x=sub[,1:(p+1)],y=sub[,p+2])
    beta=as.numeric(coef(fit,s=fit$lambda.min))[c(p+2,2:(p+1),1)]
    
    gamma=lm(sub[,1:p]~sub[,p+1])$coefficients
    
    AA[j]=beta[1]
    BB[,j]=beta[2:(p+1)]
    GG[,j]=gamma[2,]
    
    DD=DD+beta[p+2]
    TT=TT+gamma[1,]
  }
  DD=DD/t
  TT=TT/t
  par0=c(c(rbind(AA,BB,GG)),DD,TT)
  #objective function#########################################################
  obj=function(theta){
    
    fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
    AA=fen[1,]
    BB=fen[2:(1+p),]
    GG=fen[(p+2):(1+2*p),]
    DD=theta[(1+2*p)*t+1]
    TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
    ##############################################################
    f1=sum(rss(data,theta))
    f1=f1/N
    ##################################################
    f2=sum(scad(abs(AA)))+lam1*sum(1-1/(1+c*abs(BB))/(1+c*abs(GG)))
    ######################
    return(f1+f2)
  }
  #gradient#############################################################################
  gra=function(theta){
    
    fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
    AA=fen[1,]
    BB=fen[2:(1+p),]
    GG=fen[(p+2):(1+2*p),]
    DD=theta[(1+2*p)*t+1]
    TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
    ########################################################
    g1=dev(data,theta)
    g1=g1/N
    ##################################################################################
    g2=rep(0,(1+2*p)*t+1+p)
    for (j in 1:t) {
      
      len=((j-1)*(1+2*p)+1):(j*(1+2*p))
      g2[len]=c(scadd(abs(AA[j]))*sign(AA[j]),
                c*lam1*sign(BB[,j])/(1+c*abs(BB[,j]))^2/(1+c*abs(GG[,j])),
                c*lam1*sign(GG[,j])/(1+c*abs(GG[,j]))^2/(1+c*abs(BB[,j])))
    }
    ##########################
    return(g1+g2)
  }
  par=optim(par0,fn=obj,gr=gra,method="BFGS",control=list(trace=0,maxit=10000))$par
  return(round(par,2))
}