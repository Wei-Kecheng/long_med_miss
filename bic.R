bic=function(data,theta,p,t){
  
  fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
  AA=fen[1,]
  BB=fen[2:(1+p),]
  GG=fen[(p+2):(1+2*p),]
  DD=theta[(1+2*p)*t+1]
  TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
  ##########################################################################
  rss=function(data){
    
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
  ########################################################################################
  if(typeof(data)=="list"){
    f=c(0,0);N=0
    for (i in 1:length(data)) { f=f+Reduce('+',lapply(data[[i]],rss))/length(data[[i]]);N=N+nrow(data[[i]][[1]]) }
    bic1=sum(log(f/N))
  }else{
    N=nrow(data)
    bic1=sum(log(rss(data)/N))
  }
  ##########################################################################################
  judge=function(u){
    nu=0
    for (j in 1:(t-1)) {
      nu=nu+((u[j+1]!=0) & (u[j]!=0) & (u[j+1]==u[j]))
    }
    return(t-nu-sum(u==0))
  }
  
  na=judge(AA)
  nb=sum(apply(BB,1,judge))
  ng=sum(apply(GG,1,judge))
  bic2=(na+nb+ng)*log(N)/N
  ##########################
  return(bic1+bic2)
}