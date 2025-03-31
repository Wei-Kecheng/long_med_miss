MIX=function(data,p,t){
  
  if(t==4){
    t2=(data[,p+4]==2)
    t3=(data[,p+4]==3)
    t4=(data[,p+4]==4)
    #time interactions
    mx=data[,1:(p+1)]
    mx2=data[,1:(p+1)]*matrix(rep(t2,p+1),ncol=p+1)
    mx3=data[,1:(p+1)]*matrix(rep(t3,p+1),ncol=p+1)
    mx4=data[,1:(p+1)]*matrix(rep(t4,p+1),ncol=p+1)
    
    fit=cv.glmnet(x=cbind(t2,t3,t4,mx,mx2,mx3,mx4),y=data[,p+2])
    beta=as.numeric(coef(fit,s=fit$lambda.min))[-(1:t)]
    
    beta=matrix(beta,ncol=t)
    beta[,2]=beta[,1]+beta[,2]
    beta[,3]=beta[,1]+beta[,3]
    beta[,4]=beta[,1]+beta[,4]
    
    AA=beta[p+1,]
    BB=beta[-(p+1),]
    ###############################################################################
    GG=matrix(0,p,t)
    for (j in 1:p) {
      
      x=data[,p+1]
      fit=cv.glmnet(x=cbind(t2,t3,t4,x,t2*x,t3*x,t4*x),y=data[,j])
      beta=as.numeric(coef(fit,s=fit$lambda.min))[-(1:t)]
      
      GG[j,]=c(beta[1],beta[1]+beta[2],beta[1]+beta[3],beta[1]+beta[4])
    }
  }else if(t==5){
    t2=(data[,p+4]==2)
    t3=(data[,p+4]==3)
    t4=(data[,p+4]==4)
    t5=(data[,p+4]==5)
    #time interactions
    mx=data[,1:(p+1)]
    mx2=data[,1:(p+1)]*matrix(rep(t2,p+1),ncol=p+1)
    mx3=data[,1:(p+1)]*matrix(rep(t3,p+1),ncol=p+1)
    mx4=data[,1:(p+1)]*matrix(rep(t4,p+1),ncol=p+1)
    mx5=data[,1:(p+1)]*matrix(rep(t5,p+1),ncol=p+1)
    
    fit=cv.glmnet(x=cbind(t2,t3,t4,t5,mx,mx2,mx3,mx4,mx5),y=data[,p+2])
    beta=as.numeric(coef(fit,s=fit$lambda.min))[-(1:t)]
    
    beta=matrix(beta,ncol=t)
    beta[,2]=beta[,1]+beta[,2]
    beta[,3]=beta[,1]+beta[,3]
    beta[,4]=beta[,1]+beta[,4]
    beta[,5]=beta[,1]+beta[,5]
    
    AA=beta[p+1,]
    BB=beta[-(p+1),]
    ###############################################################################
    GG=matrix(0,p,t)
    for (j in 1:p) {
      
      x=data[,p+1]
      fit=cv.glmnet(x=cbind(t2,t3,t4,t5,x,t2*x,t3*x,t4*x,t5*x),y=data[,j])
      beta=as.numeric(coef(fit,s=fit$lambda.min))[-(1:t)]
      
      GG[j,]=c(beta[1],beta[1]+beta[2],beta[1]+beta[3],beta[1]+beta[4],beta[1]+beta[5])
    }
  }
  return(round(c(rbind(AA,BB,GG)),2))
}