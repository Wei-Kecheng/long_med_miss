GMM_PCA=function(data,p,t,lam1,lam2,a=3.7,c=10,patt){
  
  if(patt=="A"){ #missingness structure (A)
    data_ave=matrix(,nrow=0,ncol=p+5)
    for (i in 1:length(data)) {
      
      sub=Reduce("+",data[[i]])/length(data[[i]])
      data_ave=rbind(data_ave,sub)
    }
    ###############################################################################################
    #projection operator
    S=function(u){ -(-u>1)+(u>1)+u*(-1<=u & u<=1) }
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
    #initial########################################################################################
    AA=rep(0,t)
    BB=matrix(0,p,t)
    GG=matrix(0,p,t)
    DD=0
    TT=rep(0,p)
    for (j in 1:t) {
      
      sub=data_ave[which(data_ave[,p+4]==j),]
      
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
    par=c(c(rbind(AA,BB,GG)),DD,TT)
    #############################################################################################
    sss=1;control=1
    while(1){
      
      old=par
      #estimating functions##################################################
      moment=function(theta,kkk){
        
        fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
        AA=fen[1,]
        BB=fen[2:(1+p),]
        GG=fen[(p+2):(1+2*p),]
        DD=theta[(1+2*p)*t+1]
        TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
        
        data=data[[kkk]]
        n=nrow(data[[1]])/t
        ##################################################################
        comb=function(i){
          
          imp=data[[i]]
          x=imp[,c(p+1,1:p)]
          y=imp[,p+2]
          
          e=cbind(x,1)*matrix(rep( y-rowSums(x*cbind(rep(AA,n),matrix(rep(BB,n),ncol=p,byrow=T)))-rep(DD,n*t) ,p+2),ncol=p+2)
          e1=matrix(c(t(e[,1:(p+1)])),nrow=n,byrow=T)
          e2=rowsum(e[,p+2],rep(1:n,each=t))
          return(cbind(e1,e2))
        }
        e12=Reduce(cbind,lapply(1:length(data),comb))
        e1=e12[,-seq((p+1)*t+1,by=(p+1)*t+1,length.out=length(data))]
        e2=e12[,seq((p+1)*t+1,by=(p+1)*t+1,length.out=length(data))]
        
        if((kkk==1) | (kkk==4)) { e12=cbind(e1,e2) 
        }else if (kkk==2) { e12=cbind(e1[,-c(((p+1)*t+1):((p+1)*(2*t-1)))],rowMeans(e2)) 
        }else if (kkk==3) { e12=cbind(e1[,-c(((p+1)*(t+2)+1):((p+1)*(2*t)))],rowMeans(e2)) }
        E12=e12
        #############################################################################################
        comb=function(i){
          
          imp=data[[i]]
          x=imp[,p+1]
          y=imp[,1:p]
          
          e=y-matrix(rep(x,p),ncol=p)*matrix(rep(GG,n),ncol=p,byrow=T)-matrix(rep(TT,n*t),ncol=p,byrow=T)
          ex=matrix(rep(x,p),ncol=p)*e
          e1=matrix(c(t(ex)),nrow=n,byrow=T)
          e2=rowsum(e,rep(1:n,each=t))
          return(cbind(e1,e2))
        }
        e12=Reduce(cbind,lapply(1:length(data),comb))
        e1=e12[,-sort(sequence(nvec=rep(length(data),p),from=c((p*t+1):(p*(t+1))),by=p*(t+1)))]
        e2=e12[,sort(sequence(nvec=rep(length(data),p),from=c((p*t+1):(p*(t+1))),by=p*(t+1)))]
        
        if((kkk==1) | (kkk==4)) { e12=cbind(e1,e2) 
        }else if (kkk==2) { e12=cbind(e1[,-c((p*t+1):(p*(2*t-1)))],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==3) { e12=cbind(e1[,-c((p*(t+2)+1):(p*(2*t)))],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) }
        ##################################################################################################
        mmm=function(j){ return( e12[,seq(j,by=p,length.out=ncol(e12)/p)] ) }
        out=lapply(1:p,mmm)
        out[[p+1]]=E12
        return(out)
      }
      #derivative##################################################################################################
      derivative=function(theta,kkk){
        
        fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
        AA=fen[1,]
        BB=fen[2:(1+p),]
        GG=fen[(p+2):(1+2*p),]
        DD=theta[(1+2*p)*t+1]
        TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
        
        data=data[[kkk]]
        n=nrow(data[[1]])/t
        ###################################################################
        comb=function(i){
          
          imp=data[[i]][,c(p+1,1:p)]
          x=matrix(rep(imp,4),nrow=n*t)
          y=t(matrix(rep(bdiag(rep(list(rep(1,p+1)),t)),n),ncol=n*t))
          u=x*y
          u1=cbind(u,1)
          
          return( rbind(t(u)%*%u1,colSums(u1))/n )
        }
        d12=Reduce(rbind,lapply(1:length(data),comb))
        d1=d12[-seq((p+1)*t+1,by=(p+1)*t+1,length.out=length(data)),]
        d2=d12[seq((p+1)*t+1,by=(p+1)*t+1,length.out=length(data)),]
        
        if((kkk==1) | (kkk==4)) { d12=rbind(d1,d2)
        }else if (kkk==2) { d12=rbind(d1[-c(((1+p)*t+1):((1+p)*(2*t-1))),],colMeans(d2)) 
        }else if (kkk==3) { d12=rbind(d1[-c(((1+p)*(t+2)+1):((1+p)*(2*t))),],colMeans(d2)) }
        
        D12=matrix(0,nrow(d12),(1+2*p)*t+1+p)
        len=c(((1+2*p)*0+1):((1+2*p)*0+1+p),((1+2*p)*1+1):((1+2*p)*1+1+p),((1+2*p)*2+1):((1+2*p)*2+1+p),((1+2*p)*3+1):((1+2*p)*3+1+p),(1+2*p)*t+1)
        D12[,len]=d12
        ########################################################################################################
        imp=data[[1]][,p+1]
        x=matrix(rep(imp,4),nrow=n*t)
        y=t(matrix(rep(bdiag(rep(list(rep(1,1)),t)),n),ncol=n*t))
        u=x*y
        u1=cbind(u,1)
        d1=t(u)%*%u1/n
        d2=colSums(u1)/n
        
        if((kkk==1) | (kkk==4)) { d12=rbind(d1,d2) 
        }else if (kkk==2) { d12=rbind(d1,d1[t,],d2) 
        }else if (kkk==3) { d12=rbind(d1,d1[1:2,],d2) }
        #################################################################################################
        ddd=function(j){
          
          D12=matrix(0,nrow(d12),(1+2*p)*t+1+p)
          len=c(seq(1+p+j,(1+2*p)*(t-1)+1+p+j,length.out=t),(1+2*p)*t+1+j)
          D12[,len]=d12
          return(D12)
        }
        out=lapply(1:p,ddd)
        out[[p+1]]=D12
        return(out)
      }
      #PCA################################################################################################
      pca=function(ef){
        
        n=nrow(ef);d=ncol(ef)
        w=t(ef)%*%ef/n
        
        eg=eigen(w)
        eg1=eg$values
        eg2=eg$vectors
        
        if(abs(max(eg1)/min(eg1))>100000){
          
          tt=sum(eg1>(sum(diag(w))*log(n*d)/(n*d)))
          return(t(eg2[,1:tt]))
        }
        else{ return(diag(d)) }
      }
      ##############################################################################################################
      eo1=moment(old,1);pc1=lapply(eo1,pca)
      eo2=moment(old,2);pc2=lapply(eo2,pca)
      eo3=moment(old,3);pc3=lapply(eo3,pca)
      eo4=moment(old,4);pc4=lapply(eo4,pca)
      #objective function###########################################################################################
      obj=function(theta){
        
        ef1=moment(theta,1)
        ef2=moment(theta,2)
        ef3=moment(theta,3)
        ef4=moment(theta,4)
        #quadratic loss function
        va=function(j){
          
          return(t(pc1[[j]]%*%as.matrix(colMeans(ef1[[j]])))%*%solve(pc1[[j]]%*%t(eo1[[j]])%*%t(pc1[[j]]%*%t(eo1[[j]]))/nrow(eo1[[j]]))%*%pc1[[j]]%*%as.matrix(colMeans(ef1[[j]]))+
                   t(pc2[[j]]%*%as.matrix(colMeans(ef2[[j]])))%*%solve(pc2[[j]]%*%t(eo2[[j]])%*%t(pc2[[j]]%*%t(eo2[[j]]))/nrow(eo2[[j]]))%*%pc2[[j]]%*%as.matrix(colMeans(ef2[[j]]))+
                   t(pc3[[j]]%*%as.matrix(colMeans(ef3[[j]])))%*%solve(pc3[[j]]%*%t(eo3[[j]])%*%t(pc3[[j]]%*%t(eo3[[j]]))/nrow(eo3[[j]]))%*%pc3[[j]]%*%as.matrix(colMeans(ef3[[j]]))+
                   t(pc4[[j]]%*%as.matrix(colMeans(ef4[[j]])))%*%solve(pc4[[j]]%*%t(eo4[[j]])%*%t(pc4[[j]]%*%t(eo4[[j]]))/nrow(eo4[[j]]))%*%pc4[[j]]%*%as.matrix(colMeans(ef4[[j]])))
        }
        va=Reduce("+",lapply(1:(p+1),va))
        ########################################################################################
        fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
        AA=fen[1,]
        BB=fen[2:(1+p),]
        GG=fen[(p+2):(1+2*p),]
        DD=theta[(1+2*p)*t+1]
        TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
        #penalty for direct effects and indirect effects###############################################
        p1=sum(scad(abs(AA)))+lam1*sum(1-1/(1+c*abs(BB))/(1+c*abs(GG)))
        #fusion penalty for time-varying effects#######################################################
        dif=matrix(0,(t-1)*(1+2*p),t*(1+2*p))
        diag(dif[,-(((t-1)*(1+2*p)+1):(t*(1+2*p)))])=-1
        diag(dif[,-(1:(1+2*p))])=1
        D=cbind(dif,matrix(0,(t-1)*(1+2*p),1+p))
        
        eta=S(D%*%theta/0.0001)
        p2=lam2*(t(eta)%*%D%*%theta-0.0001/2*sum(eta^2))
        ########################################################################################
        return(va+p1+p2)
      }
      #gradient##################################################################################
      gra=function(theta){
        
        ef1=moment(theta,1);gf1=derivative(theta,1)
        ef2=moment(theta,2);gf2=derivative(theta,2)
        ef3=moment(theta,3);gf3=derivative(theta,3)
        ef4=moment(theta,4);gf4=derivative(theta,4)
        
        ga=function(j){
          
          return(t(pc1[[j]]%*%as.matrix(colMeans(ef1[[j]])))%*%solve(pc1[[j]]%*%t(eo1[[j]])%*%t(pc1[[j]]%*%t(eo1[[j]]))/nrow(eo1[[j]]))%*%pc1[[j]]%*%gf1[[j]]+
                   t(pc2[[j]]%*%as.matrix(colMeans(ef2[[j]])))%*%solve(pc2[[j]]%*%t(eo2[[j]])%*%t(pc2[[j]]%*%t(eo2[[j]]))/nrow(eo2[[j]]))%*%pc2[[j]]%*%gf2[[j]]+
                   t(pc3[[j]]%*%as.matrix(colMeans(ef3[[j]])))%*%solve(pc3[[j]]%*%t(eo3[[j]])%*%t(pc3[[j]]%*%t(eo3[[j]]))/nrow(eo3[[j]]))%*%pc3[[j]]%*%gf3[[j]]+
                   t(pc4[[j]]%*%as.matrix(colMeans(ef4[[j]])))%*%solve(pc4[[j]]%*%t(eo4[[j]])%*%t(pc4[[j]]%*%t(eo4[[j]]))/nrow(eo4[[j]]))%*%pc4[[j]]%*%gf4[[j]])
        }
        ga=Reduce("+",lapply(1:(p+1),ga))
        #########################################################################################
        fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
        AA=fen[1,]
        BB=fen[2:(1+p),]
        GG=fen[(p+2):(1+2*p),]
        DD=theta[(1+2*p)*t+1]
        TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
        #penalty for direct effects and indirect effects##########################################################
        pp1=rep(0,(1+2*p)*t+1+p)
        for (j in 1:t) {
          
          len=((j-1)*(1+2*p)+1):(j*(1+2*p))
          pp1[len]=c(scadd(abs(AA[j]))*sign(AA[j]),
                     c*lam1*sign(BB[,j])/(1+c*abs(BB[,j]))^2/(1+c*abs(GG[,j])),
                     c*lam1*sign(GG[,j])/(1+c*abs(GG[,j]))^2/(1+c*abs(BB[,j])))
        }
        #fusion penalty for time-varying effects######################################################################
        dif=matrix(0,(t-1)*(1+2*p),t*(1+2*p))
        diag(dif[,-(((t-1)*(1+2*p)+1):(t*(1+2*p)))])=-1
        diag(dif[,-(1:(1+2*p))])=1
        D=cbind(dif,matrix(0,(t-1)*(1+2*p),1+p))
        
        eta=S(D%*%theta/0.0001)
        pp2=lam2*t(D)%*%eta
        #########################################################################################################
        return(c(-2*ga)+c(pp1)+c(pp2))
      }
      ###############################################################################################################
      par=optim(old,fn=obj,gr=gra,method="BFGS",control=list(trace=0,maxit=10000))$par
      if((sum(abs(old-par))<0.000001) | (sss==control)){break}
      sss=sss+1
    }
  }else if(patt=="B"){ #missingness structure (B)
    data_ave=matrix(,nrow=0,ncol=p+5)
    for (i in 1:length(data)) {
      
      sub=Reduce("+",data[[i]])/length(data[[i]])
      data_ave=rbind(data_ave,sub)
    }
    ###############################################################################################
    S=function(u){ -(-u>1)+(u>1)+u*(-1<=u & u<=1) }
    
    scad=function(u,lam=lam1){
      lam*u*(u<=lam)+
        (a*lam*u-0.5*(u^2+lam^2))/(a-1)*((lam<u)&(u<=a*lam))+
        0.5*(a+1)*lam^2*(u>a*lam)
    }
    
    scadd=function(u,lam=lam1){
      lam*((u<=lam)+(a*lam-u)*(a*lam>u)/(a-1)/lam*(u>lam))
    }
    #initial########################################################################################
    AA=rep(0,t)
    BB=matrix(0,p,t)
    GG=matrix(0,p,t)
    DD=0
    TT=rep(0,p)
    
    for (j in 1:t) {
      
      sub=data_ave[which(data_ave[,p+4]==j),]
      
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
    par=c(c(rbind(AA,BB,GG)),DD,TT)
    #############################################################################################
    sss=1;control=1
    while(1){
      
      old=par
      moment=function(theta,kkk){
        
        fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
        AA=fen[1,]
        BB=fen[2:(1+p),]
        GG=fen[(p+2):(1+2*p),]
        DD=theta[(1+2*p)*t+1]
        TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
        
        data=data[[kkk]]
        n=nrow(data[[1]])/t
        ##################################################################
        comb=function(i){
          
          imp=data[[i]]
          x=imp[,c(p+1,1:p)]
          y=imp[,p+2]
          
          e=cbind(x,1)*matrix(rep( y-rowSums(x*cbind(rep(AA,n),matrix(rep(BB,n),ncol=p,byrow=T)))-rep(DD,n*t) ,p+2),ncol=p+2)
          e1=matrix(c(t(e[,1:(p+1)])),nrow=n,byrow=T)
          e2=rowsum(e[,p+2],rep(1:n,each=t))
          return(cbind(e1,e2))
        }
        e12=Reduce(cbind,lapply(1:length(data),comb))
        e1=e12[,-seq((p+1)*t+1,by=(p+1)*t+1,length.out=length(data))]
        e2=e12[,seq((p+1)*t+1,by=(p+1)*t+1,length.out=length(data))]
        
        if(kkk %in% c(4,5,6)) { e12=cbind(e1,e2) 
        }else if (kkk==1) { e12=cbind(e1[,-c(((p+1)*t+1):((p+1)*(2*t-1)))],rowMeans(e2)) 
        }else if (kkk==2) { e12=cbind(e1[,-c(((p+1)*(t+1)+1):((p+1)*(2*t)))],rowMeans(e2)) 
        }else if (kkk==3) { e12=cbind(e1[,-c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+3)+1):((p+1)*2*t))],rowMeans(e2)) }
        E12=e12
        #############################################################################################
        comb=function(i){
          
          imp=data[[i]]
          x=imp[,p+1]
          y=imp[,1:p]
          
          e=y-matrix(rep(x,p),ncol=p)*matrix(rep(GG,n),ncol=p,byrow=T)-matrix(rep(TT,n*t),ncol=p,byrow=T)
          ex=matrix(rep(x,p),ncol=p)*e
          e1=matrix(c(t(ex)),nrow=n,byrow=T)
          e2=rowsum(e,rep(1:n,each=t))
          return(cbind(e1,e2))
        }
        e12=Reduce(cbind,lapply(1:length(data),comb))
        e1=e12[,-sort(sequence(nvec=rep(length(data),p),from=c((p*t+1):(p*(t+1))),by=p*(t+1)))]
        e2=e12[,sort(sequence(nvec=rep(length(data),p),from=c((p*t+1):(p*(t+1))),by=p*(t+1)))]
        
        if(kkk %in% c(4,5,6)) { e12=cbind(e1,e2) 
        }else if (kkk==1) { e12=cbind(e1[,-c((p*t+1):(p*(2*t-1)))],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==2) { e12=cbind(e1[,-c((p*(t+1)+1):(p*(2*t)))],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==3) { e12=cbind(e1[,-c((p*t+1):(p*(t+1)),(p*(t+3)+1):(p*2*t))],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) }
        ##################################################################################################
        mmm=function(j){ return( e12[,seq(j,by=p,length.out=ncol(e12)/p)] ) }
        out=lapply(1:p,mmm)
        out[[p+1]]=E12
        return(out)
      }
      ###################################################################################################
      derivative=function(theta,kkk){
        
        fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
        AA=fen[1,]
        BB=fen[2:(1+p),]
        GG=fen[(p+2):(1+2*p),]
        DD=theta[(1+2*p)*t+1]
        TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
        
        data=data[[kkk]]
        n=nrow(data[[1]])/t
        ###################################################################
        comb=function(i){
          
          imp=data[[i]][,c(p+1,1:p)]
          x=matrix(rep(imp,4),nrow=n*t)
          y=t(matrix(rep(bdiag(rep(list(rep(1,p+1)),t)),n),ncol=n*t))
          u=x*y
          u1=cbind(u,1)
          
          return( rbind(t(u)%*%u1,colSums(u1))/n )
        }
        d12=Reduce(rbind,lapply(1:length(data),comb))
        d1=d12[-seq((p+1)*t+1,by=(p+1)*t+1,length.out=length(data)),]
        d2=d12[seq((p+1)*t+1,by=(p+1)*t+1,length.out=length(data)),]
        
        if(kkk %in% c(4,5,6)) { d12=rbind(d1,d2) 
        }else if (kkk==1) { d12=rbind(d1[-c(((p+1)*t+1):((p+1)*(2*t-1))),],colMeans(d2)) 
        }else if (kkk==2) { d12=rbind(d1[-c(((p+1)*(t+1)+1):((p+1)*(2*t))),],colMeans(d2)) 
        }else if (kkk==3) { d12=rbind(d1[-c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+3)+1):((p+1)*2*t)),],colMeans(d2)) }
        
        D12=matrix(0,nrow(d12),(1+2*p)*t+1+p)
        len=c(((1+2*p)*0+1):((1+2*p)*0+1+p),((1+2*p)*1+1):((1+2*p)*1+1+p),((1+2*p)*2+1):((1+2*p)*2+1+p),((1+2*p)*3+1):((1+2*p)*3+1+p),(1+2*p)*t+1)
        D12[,len]=d12
        ########################################################################################################
        imp=data[[1]][,p+1]
        x=matrix(rep(imp,4),nrow=n*t)
        y=t(matrix(rep(bdiag(rep(list(rep(1,1)),t)),n),ncol=n*t))
        u=x*y
        u1=cbind(u,1)
        d1=t(u)%*%u1/n
        d2=colSums(u1)/n
        
        if(kkk %in% c(4,5,6)) { d12=rbind(d1,d2) 
        }else if (kkk==1) { d12=rbind(d1,d1[t,],d2) 
        }else if (kkk==2) { d12=rbind(d1,d1[1,],d2) 
        }else if (kkk==3) { d12=rbind(d1,d1[2:3,],d2) }
        #################################################################################################
        ddd=function(j){
          
          D12=matrix(0,nrow(d12),(1+2*p)*t+1+p)
          len=c(seq(1+p+j,(1+2*p)*(t-1)+1+p+j,length.out=t),(1+2*p)*t+1+j)
          D12[,len]=d12
          return(D12)
        }
        out=lapply(1:p,ddd)
        out[[p+1]]=D12
        return(out)
      }
      #################################################################################################
      pca=function(ef){
        
        n=nrow(ef);d=ncol(ef)
        w=t(ef)%*%ef/n
        
        eg=eigen(w)
        eg1=eg$values
        eg2=eg$vectors
        
        if(abs(max(eg1)/min(eg1))>100000){
          
          tt=sum(eg1>(sum(diag(w))*log(n*d)/(n*d)))
          return(t(eg2[,1:tt]))
        }
        else{ return(diag(d)) }
      }
      ##############################################################################################################
      eo1=moment(old,1);pc1=lapply(eo1,pca)
      eo2=moment(old,2);pc2=lapply(eo2,pca)
      eo3=moment(old,3);pc3=lapply(eo3,pca)
      eo4=moment(old,4);pc4=lapply(eo4,pca)
      eo5=moment(old,5);pc5=lapply(eo5,pca)
      eo6=moment(old,6);pc6=lapply(eo6,pca)
      ##############################################################################################################
      obj=function(theta){
        
        ef1=moment(theta,1)
        ef2=moment(theta,2)
        ef3=moment(theta,3)
        ef4=moment(theta,4)
        ef5=moment(theta,5)
        ef6=moment(theta,6)
        
        va=function(j){
          
          return(t(pc1[[j]]%*%as.matrix(colMeans(ef1[[j]])))%*%solve(pc1[[j]]%*%t(eo1[[j]])%*%t(pc1[[j]]%*%t(eo1[[j]]))/nrow(eo1[[j]]))%*%pc1[[j]]%*%as.matrix(colMeans(ef1[[j]]))+
                   t(pc2[[j]]%*%as.matrix(colMeans(ef2[[j]])))%*%solve(pc2[[j]]%*%t(eo2[[j]])%*%t(pc2[[j]]%*%t(eo2[[j]]))/nrow(eo2[[j]]))%*%pc2[[j]]%*%as.matrix(colMeans(ef2[[j]]))+
                   t(pc3[[j]]%*%as.matrix(colMeans(ef3[[j]])))%*%solve(pc3[[j]]%*%t(eo3[[j]])%*%t(pc3[[j]]%*%t(eo3[[j]]))/nrow(eo3[[j]]))%*%pc3[[j]]%*%as.matrix(colMeans(ef3[[j]]))+
                   t(pc4[[j]]%*%as.matrix(colMeans(ef4[[j]])))%*%solve(pc4[[j]]%*%t(eo4[[j]])%*%t(pc4[[j]]%*%t(eo4[[j]]))/nrow(eo4[[j]]))%*%pc4[[j]]%*%as.matrix(colMeans(ef4[[j]]))+
                   t(pc5[[j]]%*%as.matrix(colMeans(ef5[[j]])))%*%solve(pc5[[j]]%*%t(eo5[[j]])%*%t(pc5[[j]]%*%t(eo5[[j]]))/nrow(eo5[[j]]))%*%pc5[[j]]%*%as.matrix(colMeans(ef5[[j]]))+
                   t(pc6[[j]]%*%as.matrix(colMeans(ef6[[j]])))%*%solve(pc6[[j]]%*%t(eo6[[j]])%*%t(pc6[[j]]%*%t(eo6[[j]]))/nrow(eo6[[j]]))%*%pc6[[j]]%*%as.matrix(colMeans(ef6[[j]])))
        }
        va=Reduce("+",lapply(1:(p+1),va))
        ########################################################################################
        fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
        AA=fen[1,]
        BB=fen[2:(1+p),]
        GG=fen[(p+2):(1+2*p),]
        DD=theta[(1+2*p)*t+1]
        TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
        ######################################################################################
        p1=sum(scad(abs(AA)))+lam1*sum(1-1/(1+c*abs(BB))/(1+c*abs(GG)))
        ########################################################
        dif=matrix(0,(t-1)*(1+2*p),t*(1+2*p))
        diag(dif[,-(((t-1)*(1+2*p)+1):(t*(1+2*p)))])=-1
        diag(dif[,-(1:(1+2*p))])=1
        D=cbind(dif,matrix(0,(t-1)*(1+2*p),1+p))
        
        eta=S(D%*%theta/0.0001)
        p2=lam2*(t(eta)%*%D%*%theta-0.0001/2*sum(eta^2))
        ########################################################################################
        return(va+p1+p2)
      }
      ##############################################################################################################
      gra=function(theta){
        
        ef1=moment(theta,1);gf1=derivative(theta,1)
        ef2=moment(theta,2);gf2=derivative(theta,2)
        ef3=moment(theta,3);gf3=derivative(theta,3)
        ef4=moment(theta,4);gf4=derivative(theta,4)
        ef5=moment(theta,5);gf5=derivative(theta,5)
        ef6=moment(theta,6);gf6=derivative(theta,6)
        
        ga=function(j){
          
          return(t(pc1[[j]]%*%as.matrix(colMeans(ef1[[j]])))%*%solve(pc1[[j]]%*%t(eo1[[j]])%*%t(pc1[[j]]%*%t(eo1[[j]]))/nrow(eo1[[j]]))%*%pc1[[j]]%*%gf1[[j]]+
                   t(pc2[[j]]%*%as.matrix(colMeans(ef2[[j]])))%*%solve(pc2[[j]]%*%t(eo2[[j]])%*%t(pc2[[j]]%*%t(eo2[[j]]))/nrow(eo2[[j]]))%*%pc2[[j]]%*%gf2[[j]]+
                   t(pc3[[j]]%*%as.matrix(colMeans(ef3[[j]])))%*%solve(pc3[[j]]%*%t(eo3[[j]])%*%t(pc3[[j]]%*%t(eo3[[j]]))/nrow(eo3[[j]]))%*%pc3[[j]]%*%gf3[[j]]+
                   t(pc4[[j]]%*%as.matrix(colMeans(ef4[[j]])))%*%solve(pc4[[j]]%*%t(eo4[[j]])%*%t(pc4[[j]]%*%t(eo4[[j]]))/nrow(eo4[[j]]))%*%pc4[[j]]%*%gf4[[j]]+
                   t(pc5[[j]]%*%as.matrix(colMeans(ef5[[j]])))%*%solve(pc5[[j]]%*%t(eo5[[j]])%*%t(pc5[[j]]%*%t(eo5[[j]]))/nrow(eo5[[j]]))%*%pc5[[j]]%*%gf5[[j]]+
                   t(pc6[[j]]%*%as.matrix(colMeans(ef6[[j]])))%*%solve(pc6[[j]]%*%t(eo6[[j]])%*%t(pc6[[j]]%*%t(eo6[[j]]))/nrow(eo6[[j]]))%*%pc6[[j]]%*%gf6[[j]])
        }
        ga=Reduce("+",lapply(1:(p+1),ga))
        #########################################################################################
        fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
        AA=fen[1,]
        BB=fen[2:(1+p),]
        GG=fen[(p+2):(1+2*p),]
        DD=theta[(1+2*p)*t+1]
        TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
        ##################################################################################
        pp1=rep(0,(1+2*p)*t+1+p)
        for (j in 1:t) {
          
          len=((j-1)*(1+2*p)+1):(j*(1+2*p))
          pp1[len]=c(scadd(abs(AA[j]))*sign(AA[j]),
                     c*lam1*sign(BB[,j])/(1+c*abs(BB[,j]))^2/(1+c*abs(GG[,j])),
                     c*lam1*sign(GG[,j])/(1+c*abs(GG[,j]))^2/(1+c*abs(BB[,j])))
        }
        ########################################################################################################
        dif=matrix(0,(t-1)*(1+2*p),t*(1+2*p))
        diag(dif[,-(((t-1)*(1+2*p)+1):(t*(1+2*p)))])=-1
        diag(dif[,-(1:(1+2*p))])=1
        D=cbind(dif,matrix(0,(t-1)*(1+2*p),1+p))
        
        eta=S(D%*%theta/0.0001)
        pp2=lam2*t(D)%*%eta
        #########################################################################################################
        return(c(-2*ga)+c(pp1)+c(pp2))
      }
      par=optim(old,fn=obj,gr=gra,method="BFGS",control=list(trace=0,maxit=10000))$par
      if((sum(abs(old-par))<0.000001) | (sss==control)){break}
      sss=sss+1
    }
  }else if(patt=="T4"){ #missingness structure (T4)
    data_ave=matrix(,nrow=0,ncol=p+5)
    for (i in 1:length(data)) {
      
      sub=Reduce("+",data[[i]])/length(data[[i]])
      data_ave=rbind(data_ave,sub)
    }
    ###############################################################################################
    S=function(u){ -(-u>1)+(u>1)+u*(-1<=u & u<=1) }
    
    scad=function(u,lam=lam1){
      lam*u*(u<=lam)+
        (a*lam*u-0.5*(u^2+lam^2))/(a-1)*((lam<u)&(u<=a*lam))+
        0.5*(a+1)*lam^2*(u>a*lam)
    }
    
    scadd=function(u,lam=lam1){
      lam*((u<=lam)+(a*lam-u)*(a*lam>u)/(a-1)/lam*(u>lam))
    }
    #initial########################################################################################
    AA=rep(0,t)
    BB=matrix(0,p,t)
    GG=matrix(0,p,t)
    DD=0
    TT=rep(0,p)
    
    for (j in 1:t) {
      
      sub=data_ave[which(data_ave[,p+4]==j),]
      
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
    par=c(c(rbind(AA,BB,GG)),DD,TT)
    ########################################################################
    sss=1;control=1
    while(1){
      
      old=par
      moment=function(theta,kkk){
        
        fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
        AA=fen[1,]
        BB=fen[2:(1+p),]
        GG=fen[(p+2):(1+2*p),]
        DD=theta[(1+2*p)*t+1]
        TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
        
        data=data[[kkk]]
        n=nrow(data[[1]])/t
        ##################################################################
        comb=function(i){
          
          imp=data[[i]]
          x=imp[,c(p+1,1:p)]
          y=imp[,p+2]
          
          e=cbind(x,1)*matrix(rep( y-rowSums(x*cbind(rep(AA,n),matrix(rep(BB,n),ncol=p,byrow=T)))-rep(DD,n*t) ,p+2),ncol=p+2)
          e1=matrix(c(t(e[,1:(p+1)])),nrow=n,byrow=T)
          e2=rowsum(e[,p+2],rep(1:n,each=t))
          return(cbind(e1,e2))
        }
        e12=Reduce(cbind,lapply(1:length(data),comb))
        e1=e12[,-seq((p+1)*t+1,by=(p+1)*t+1,length.out=length(data))]
        e2=e12[,seq((p+1)*t+1,by=(p+1)*t+1,length.out=length(data))]
        
        if(kkk %in% c(1,12,13,14,15)) { e12=cbind(e1,e2) 
        }else if (kkk==2) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+3))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) 
        }else if (kkk==3) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+2)),((p+1)*(t+3)+1):((p+1)*(t+4))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) 
        }else if (kkk==4) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+2)+1):((p+1)*(t+4))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) 
        }else if (kkk==5) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+4))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) 
        }else if (kkk==6) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+2))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) 
        }else if (kkk==7) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+2)+1):((p+1)*(t+3))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) 
        }else if (kkk==8) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+3)+1):((p+1)*(t+4))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) 
        }else if (kkk==9) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+3))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) 
        }else if (kkk==10) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+2)),((p+1)*(t+3)+1):((p+1)*(t+4))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) 
        }else if (kkk==11) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+2)+1):((p+1)*(t+4))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) }
        E12=e12
        #############################################################################################
        comb=function(i){
          
          imp=data[[i]]
          x=imp[,p+1]
          y=imp[,1:p]
          
          e=y-matrix(rep(x,p),ncol=p)*matrix(rep(GG,n),ncol=p,byrow=T)-matrix(rep(TT,n*t),ncol=p,byrow=T)
          ex=matrix(rep(x,p),ncol=p)*e
          e1=matrix(c(t(ex)),nrow=n,byrow=T)
          e2=rowsum(e,rep(1:n,each=t))
          return(cbind(e1,e2))
        }
        e12=Reduce(cbind,lapply(1:length(data),comb))
        e1=e12[,-sort(sequence(nvec=rep(length(data),p),from=c((p*t+1):(p*(t+1))),by=p*(t+1)))]
        e2=e12[,sort(sequence(nvec=rep(length(data),p),from=c((p*t+1):(p*(t+1))),by=p*(t+1)))]
        
        if(kkk %in% c(1,12,13,14,15)) { e12=cbind(e1,e2) 
        }else if (kkk==2) { len=sort(sequence(nvec=rep(length(data)-1,3*p),from=c((p*t+1):(p*(t+3))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==3) { len=sort(sequence(nvec=rep(length(data)-1,3*p),from=c((p*t+1):(p*(t+2)),(p*(t+3)+1):(p*(t+4))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==4) { len=sort(sequence(nvec=rep(length(data)-1,3*p),from=c((p*t+1):(p*(t+1)),(p*(t+2)+1):(p*(t+4))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==5) { len=sort(sequence(nvec=rep(length(data)-1,3*p),from=c((p*(t+1)+1):(p*(t+4))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==6) { len=sort(sequence(nvec=rep(length(data)-1,2*p),from=c((p*t+1):(p*(t+2))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==7) { len=sort(sequence(nvec=rep(length(data)-1,2*p),from=c((p*t+1):(p*(t+1)),(p*(t+2)+1):(p*(t+3))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==8) { len=sort(sequence(nvec=rep(length(data)-1,2*p),from=c((p*t+1):(p*(t+1)),(p*(t+3)+1):(p*(t+4))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==9) { len=sort(sequence(nvec=rep(length(data)-1,2*p),from=c((p*(t+1)+1):(p*(t+3))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==10) { len=sort(sequence(nvec=rep(length(data)-1,2*p),from=c((p*(t+1)+1):(p*(t+2)),(p*(t+3)+1):(p*(t+4))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==11) { len=sort(sequence(nvec=rep(length(data)-1,2*p),from=c((p*(t+2)+1):(p*(t+4))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) }
        ##################################################################################################
        mmm=function(j){ return( e12[,seq(j,by=p,length.out=ncol(e12)/p)] ) }
        out=lapply(1:p,mmm)
        out[[p+1]]=E12
        return(out)
      }
      ###################################################################################################
      derivative=function(theta,kkk){
        
        fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
        AA=fen[1,]
        BB=fen[2:(1+p),]
        GG=fen[(p+2):(1+2*p),]
        DD=theta[(1+2*p)*t+1]
        TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
        
        data=data[[kkk]]
        n=nrow(data[[1]])/t
        ###################################################################
        comb=function(i){
          
          imp=data[[i]][,c(p+1,1:p)]
          x=matrix(rep(imp,4),nrow=n*t)
          y=t(matrix(rep(bdiag(rep(list(rep(1,p+1)),t)),n),ncol=n*t))
          u=x*y
          u1=cbind(u,1)
          
          return( rbind(t(u)%*%u1,colSums(u1))/n )
        }
        d12=Reduce(rbind,lapply(1:length(data),comb))
        d1=d12[-seq((p+1)*t+1,by=(p+1)*t+1,length.out=length(data)),]
        d2=d12[seq((p+1)*t+1,by=(p+1)*t+1,length.out=length(data)),]
        
        if(kkk %in% c(1,12,13,14,15)) { d12=rbind(d1,d2) 
        }else if (kkk==2) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+3))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) 
        }else if (kkk==3) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+2)),((p+1)*(t+3)+1):((p+1)*(t+4))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) 
        }else if (kkk==4) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+2)+1):((p+1)*(t+4))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) 
        }else if (kkk==5) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+4))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) 
        }else if (kkk==6) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+2))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) 
        }else if (kkk==7) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+2)+1):((p+1)*(t+3))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) 
        }else if (kkk==8) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+3)+1):((p+1)*(t+4))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) 
        }else if (kkk==9) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+3))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) 
        }else if (kkk==10) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+2)),((p+1)*(t+3)+1):((p+1)*(t+4))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) 
        }else if (kkk==11) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+2)+1):((p+1)*(t+4))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) }
        
        D12=matrix(0,nrow(d12),(1+2*p)*t+1+p)
        len=c(((1+2*p)*0+1):((1+2*p)*0+1+p),((1+2*p)*1+1):((1+2*p)*1+1+p),((1+2*p)*2+1):((1+2*p)*2+1+p),((1+2*p)*3+1):((1+2*p)*3+1+p),(1+2*p)*t+1)
        D12[,len]=d12
        ########################################################################################################
        imp=data[[1]][,p+1]
        x=matrix(rep(imp,4),nrow=n*t)
        y=t(matrix(rep(bdiag(rep(list(rep(1,1)),t)),n),ncol=n*t))
        u=x*y
        u1=cbind(u,1)
        d1=t(u)%*%u1/n
        d2=colSums(u1)/n
        
        if(kkk %in% c(1,12,13,14,15)) { d12=rbind(d1,d2) 
        }else if (kkk==2) { d12=rbind(d1,matrix(rep(d1[4,],length(data)-1),ncol=ncol(d1),byrow=T),d2) 
        }else if (kkk==3) { d12=rbind(d1,matrix(rep(d1[3,],length(data)-1),ncol=ncol(d1),byrow=T),d2) 
        }else if (kkk==4) { d12=rbind(d1,matrix(rep(d1[2,],length(data)-1),ncol=ncol(d1),byrow=T),d2) 
        }else if (kkk==5) { d12=rbind(d1,matrix(rep(d1[1,],length(data)-1),ncol=ncol(d1),byrow=T),d2) 
        }else if (kkk==6) { d12=rbind(d1,matrix(rep(t(d1[c(3,4),]),length(data)-1),ncol=ncol(d1),byrow=T),d2) 
        }else if (kkk==7) { d12=rbind(d1,matrix(rep(t(d1[c(2,4),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==8) { d12=rbind(d1,matrix(rep(t(d1[c(2,3),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==9) { d12=rbind(d1,matrix(rep(t(d1[c(1,4),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==10) { d12=rbind(d1,matrix(rep(t(d1[c(1,3),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==11) { d12=rbind(d1,matrix(rep(t(d1[c(1,2),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)}
        #################################################################################################
        ddd=function(j){
          
          D12=matrix(0,nrow(d12),(1+2*p)*t+1+p)
          len=c(seq(1+p+j,(1+2*p)*(t-1)+1+p+j,length.out=t),(1+2*p)*t+1+j)
          D12[,len]=d12
          return(D12)
        }
        out=lapply(1:p,ddd)
        out[[p+1]]=D12
        return(out)
      }
      #################################################################################################
      pca=function(ef){
        
        n=nrow(ef);d=ncol(ef)
        w=t(ef)%*%ef/n
        
        eg=eigen(w)
        eg1=eg$values
        eg2=eg$vectors
        
        if(abs(max(eg1)/min(eg1))>100000){
          
          tt=sum(eg1>(sum(diag(w))*log(n*d)/(n*d)))
          return(t(eg2[,1:tt]))
        }
        else{ return(diag(d)) }
      }
      ##############################################################################################################
      eo1=moment(old,1);pc1=lapply(eo1,pca)
      eo2=moment(old,2);pc2=lapply(eo2,pca)
      eo3=moment(old,3);pc3=lapply(eo3,pca)
      eo4=moment(old,4);pc4=lapply(eo4,pca)
      eo5=moment(old,5);pc5=lapply(eo5,pca)
      eo6=moment(old,6);pc6=lapply(eo6,pca)
      eo7=moment(old,7);pc7=lapply(eo7,pca)
      eo8=moment(old,8);pc8=lapply(eo8,pca)
      eo9=moment(old,9);pc9=lapply(eo9,pca)
      eo10=moment(old,10);pc10=lapply(eo10,pca)
      eo11=moment(old,11);pc11=lapply(eo11,pca)
      eo12=moment(old,12);pc12=lapply(eo12,pca)
      eo13=moment(old,13);pc13=lapply(eo13,pca)
      eo14=moment(old,14);pc14=lapply(eo14,pca)
      eo15=moment(old,15);pc15=lapply(eo15,pca)
      ##############################################################################################################
      obj=function(theta){
        
        ef1=moment(theta,1)
        ef2=moment(theta,2)
        ef3=moment(theta,3)
        ef4=moment(theta,4)
        ef5=moment(theta,5)
        ef6=moment(theta,6)
        ef7=moment(theta,7)
        ef8=moment(theta,8)
        ef9=moment(theta,9)
        ef10=moment(theta,10)
        ef11=moment(theta,11)
        ef12=moment(theta,12)
        ef13=moment(theta,13)
        ef14=moment(theta,14)
        ef15=moment(theta,15)
        
        va=function(j){
          return(t(pc1[[j]]%*%as.matrix(colMeans(ef1[[j]])))%*%solve(pc1[[j]]%*%t(eo1[[j]])%*%t(pc1[[j]]%*%t(eo1[[j]]))/nrow(eo1[[j]]))%*%pc1[[j]]%*%as.matrix(colMeans(ef1[[j]]))+
                   t(pc2[[j]]%*%as.matrix(colMeans(ef2[[j]])))%*%solve(pc2[[j]]%*%t(eo2[[j]])%*%t(pc2[[j]]%*%t(eo2[[j]]))/nrow(eo2[[j]]))%*%pc2[[j]]%*%as.matrix(colMeans(ef2[[j]]))+
                   t(pc3[[j]]%*%as.matrix(colMeans(ef3[[j]])))%*%solve(pc3[[j]]%*%t(eo3[[j]])%*%t(pc3[[j]]%*%t(eo3[[j]]))/nrow(eo3[[j]]))%*%pc3[[j]]%*%as.matrix(colMeans(ef3[[j]]))+
                   t(pc4[[j]]%*%as.matrix(colMeans(ef4[[j]])))%*%solve(pc4[[j]]%*%t(eo4[[j]])%*%t(pc4[[j]]%*%t(eo4[[j]]))/nrow(eo4[[j]]))%*%pc4[[j]]%*%as.matrix(colMeans(ef4[[j]]))+
                   t(pc5[[j]]%*%as.matrix(colMeans(ef5[[j]])))%*%solve(pc5[[j]]%*%t(eo5[[j]])%*%t(pc5[[j]]%*%t(eo5[[j]]))/nrow(eo5[[j]]))%*%pc5[[j]]%*%as.matrix(colMeans(ef5[[j]]))+
                   t(pc6[[j]]%*%as.matrix(colMeans(ef6[[j]])))%*%solve(pc6[[j]]%*%t(eo6[[j]])%*%t(pc6[[j]]%*%t(eo6[[j]]))/nrow(eo6[[j]]))%*%pc6[[j]]%*%as.matrix(colMeans(ef6[[j]]))+
                   t(pc7[[j]]%*%as.matrix(colMeans(ef7[[j]])))%*%solve(pc7[[j]]%*%t(eo7[[j]])%*%t(pc7[[j]]%*%t(eo7[[j]]))/nrow(eo7[[j]]))%*%pc7[[j]]%*%as.matrix(colMeans(ef7[[j]]))+
                   t(pc8[[j]]%*%as.matrix(colMeans(ef8[[j]])))%*%solve(pc8[[j]]%*%t(eo8[[j]])%*%t(pc8[[j]]%*%t(eo8[[j]]))/nrow(eo8[[j]]))%*%pc8[[j]]%*%as.matrix(colMeans(ef8[[j]]))+
                   t(pc9[[j]]%*%as.matrix(colMeans(ef9[[j]])))%*%solve(pc9[[j]]%*%t(eo9[[j]])%*%t(pc9[[j]]%*%t(eo9[[j]]))/nrow(eo9[[j]]))%*%pc9[[j]]%*%as.matrix(colMeans(ef9[[j]]))+
                   t(pc10[[j]]%*%as.matrix(colMeans(ef10[[j]])))%*%solve(pc10[[j]]%*%t(eo10[[j]])%*%t(pc10[[j]]%*%t(eo10[[j]]))/nrow(eo10[[j]]))%*%pc10[[j]]%*%as.matrix(colMeans(ef10[[j]]))+
                   t(pc11[[j]]%*%as.matrix(colMeans(ef11[[j]])))%*%solve(pc11[[j]]%*%t(eo11[[j]])%*%t(pc11[[j]]%*%t(eo11[[j]]))/nrow(eo11[[j]]))%*%pc11[[j]]%*%as.matrix(colMeans(ef11[[j]]))+
                   t(pc12[[j]]%*%as.matrix(colMeans(ef12[[j]])))%*%solve(pc12[[j]]%*%t(eo12[[j]])%*%t(pc12[[j]]%*%t(eo12[[j]]))/nrow(eo12[[j]]))%*%pc12[[j]]%*%as.matrix(colMeans(ef12[[j]]))+
                   t(pc13[[j]]%*%as.matrix(colMeans(ef13[[j]])))%*%solve(pc13[[j]]%*%t(eo13[[j]])%*%t(pc13[[j]]%*%t(eo13[[j]]))/nrow(eo13[[j]]))%*%pc13[[j]]%*%as.matrix(colMeans(ef13[[j]]))+
                   t(pc14[[j]]%*%as.matrix(colMeans(ef14[[j]])))%*%solve(pc14[[j]]%*%t(eo14[[j]])%*%t(pc14[[j]]%*%t(eo14[[j]]))/nrow(eo14[[j]]))%*%pc14[[j]]%*%as.matrix(colMeans(ef14[[j]]))+
                   t(pc15[[j]]%*%as.matrix(colMeans(ef15[[j]])))%*%solve(pc15[[j]]%*%t(eo15[[j]])%*%t(pc15[[j]]%*%t(eo15[[j]]))/nrow(eo15[[j]]))%*%pc15[[j]]%*%as.matrix(colMeans(ef15[[j]])))
        }
        va=Reduce("+",lapply(1:(p+1),va))
        ########################################################################################
        fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
        AA=fen[1,]
        BB=fen[2:(1+p),]
        GG=fen[(p+2):(1+2*p),]
        DD=theta[(1+2*p)*t+1]
        TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
        ######################################################################################
        p1=sum(scad(abs(AA)))+lam1*sum(1-1/(1+c*abs(BB))/(1+c*abs(GG)))
        ########################################################
        dif=matrix(0,(t-1)*(1+2*p),t*(1+2*p))
        diag(dif[,-(((t-1)*(1+2*p)+1):(t*(1+2*p)))])=-1
        diag(dif[,-(1:(1+2*p))])=1
        D=cbind(dif,matrix(0,(t-1)*(1+2*p),1+p))
        
        eta=S(D%*%theta/0.0001)
        p2=lam2*(t(eta)%*%D%*%theta-0.0001/2*sum(eta^2))
        ########################################################################################
        return(va+p1+p2)
      }
      ##############################################################################################################
      gra=function(theta){
        
        ef1=moment(theta,1);gf1=derivative(theta,1)
        ef2=moment(theta,2);gf2=derivative(theta,2)
        ef3=moment(theta,3);gf3=derivative(theta,3)
        ef4=moment(theta,4);gf4=derivative(theta,4)
        ef5=moment(theta,5);gf5=derivative(theta,5)
        ef6=moment(theta,6);gf6=derivative(theta,6)
        ef7=moment(theta,7);gf7=derivative(theta,7)
        ef8=moment(theta,8);gf8=derivative(theta,8)
        ef9=moment(theta,9);gf9=derivative(theta,9)
        ef10=moment(theta,10);gf10=derivative(theta,10)
        ef11=moment(theta,11);gf11=derivative(theta,11)
        ef12=moment(theta,12);gf12=derivative(theta,12)
        ef13=moment(theta,13);gf13=derivative(theta,13)
        ef14=moment(theta,14);gf14=derivative(theta,14)
        ef15=moment(theta,15);gf15=derivative(theta,15)
        
        ga=function(j){
          return(t(pc1[[j]]%*%as.matrix(colMeans(ef1[[j]])))%*%solve(pc1[[j]]%*%t(eo1[[j]])%*%t(pc1[[j]]%*%t(eo1[[j]]))/nrow(eo1[[j]]))%*%pc1[[j]]%*%gf1[[j]]+
                   t(pc2[[j]]%*%as.matrix(colMeans(ef2[[j]])))%*%solve(pc2[[j]]%*%t(eo2[[j]])%*%t(pc2[[j]]%*%t(eo2[[j]]))/nrow(eo2[[j]]))%*%pc2[[j]]%*%gf2[[j]]+
                   t(pc3[[j]]%*%as.matrix(colMeans(ef3[[j]])))%*%solve(pc3[[j]]%*%t(eo3[[j]])%*%t(pc3[[j]]%*%t(eo3[[j]]))/nrow(eo3[[j]]))%*%pc3[[j]]%*%gf3[[j]]+
                   t(pc4[[j]]%*%as.matrix(colMeans(ef4[[j]])))%*%solve(pc4[[j]]%*%t(eo4[[j]])%*%t(pc4[[j]]%*%t(eo4[[j]]))/nrow(eo4[[j]]))%*%pc4[[j]]%*%gf4[[j]]+
                   t(pc5[[j]]%*%as.matrix(colMeans(ef5[[j]])))%*%solve(pc5[[j]]%*%t(eo5[[j]])%*%t(pc5[[j]]%*%t(eo5[[j]]))/nrow(eo5[[j]]))%*%pc5[[j]]%*%gf5[[j]]+
                   t(pc6[[j]]%*%as.matrix(colMeans(ef6[[j]])))%*%solve(pc6[[j]]%*%t(eo6[[j]])%*%t(pc6[[j]]%*%t(eo6[[j]]))/nrow(eo6[[j]]))%*%pc6[[j]]%*%gf6[[j]]+
                   t(pc7[[j]]%*%as.matrix(colMeans(ef7[[j]])))%*%solve(pc7[[j]]%*%t(eo7[[j]])%*%t(pc7[[j]]%*%t(eo7[[j]]))/nrow(eo7[[j]]))%*%pc7[[j]]%*%gf7[[j]]+
                   t(pc8[[j]]%*%as.matrix(colMeans(ef8[[j]])))%*%solve(pc8[[j]]%*%t(eo8[[j]])%*%t(pc8[[j]]%*%t(eo8[[j]]))/nrow(eo8[[j]]))%*%pc8[[j]]%*%gf8[[j]]+
                   t(pc9[[j]]%*%as.matrix(colMeans(ef9[[j]])))%*%solve(pc9[[j]]%*%t(eo9[[j]])%*%t(pc9[[j]]%*%t(eo9[[j]]))/nrow(eo9[[j]]))%*%pc9[[j]]%*%gf9[[j]]+
                   t(pc10[[j]]%*%as.matrix(colMeans(ef10[[j]])))%*%solve(pc10[[j]]%*%t(eo10[[j]])%*%t(pc10[[j]]%*%t(eo10[[j]]))/nrow(eo10[[j]]))%*%pc10[[j]]%*%gf10[[j]]+
                   t(pc11[[j]]%*%as.matrix(colMeans(ef11[[j]])))%*%solve(pc11[[j]]%*%t(eo11[[j]])%*%t(pc11[[j]]%*%t(eo11[[j]]))/nrow(eo11[[j]]))%*%pc11[[j]]%*%gf11[[j]]+
                   t(pc12[[j]]%*%as.matrix(colMeans(ef12[[j]])))%*%solve(pc12[[j]]%*%t(eo12[[j]])%*%t(pc12[[j]]%*%t(eo12[[j]]))/nrow(eo12[[j]]))%*%pc12[[j]]%*%gf12[[j]]+
                   t(pc13[[j]]%*%as.matrix(colMeans(ef13[[j]])))%*%solve(pc13[[j]]%*%t(eo13[[j]])%*%t(pc13[[j]]%*%t(eo13[[j]]))/nrow(eo13[[j]]))%*%pc13[[j]]%*%gf13[[j]]+
                   t(pc14[[j]]%*%as.matrix(colMeans(ef14[[j]])))%*%solve(pc14[[j]]%*%t(eo14[[j]])%*%t(pc14[[j]]%*%t(eo14[[j]]))/nrow(eo14[[j]]))%*%pc14[[j]]%*%gf14[[j]]+
                   t(pc15[[j]]%*%as.matrix(colMeans(ef15[[j]])))%*%solve(pc15[[j]]%*%t(eo15[[j]])%*%t(pc15[[j]]%*%t(eo15[[j]]))/nrow(eo15[[j]]))%*%pc15[[j]]%*%gf15[[j]])
        }
        ga=Reduce("+",lapply(1:(p+1),ga))
        #########################################################################################
        fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
        AA=fen[1,]
        BB=fen[2:(1+p),]
        GG=fen[(p+2):(1+2*p),]
        DD=theta[(1+2*p)*t+1]
        TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
        ##################################################################################
        pp1=rep(0,(1+2*p)*t+1+p)
        for (j in 1:t) {
          
          len=((j-1)*(1+2*p)+1):(j*(1+2*p))
          pp1[len]=c(scadd(abs(AA[j]))*sign(AA[j]),
                     c*lam1*sign(BB[,j])/(1+c*abs(BB[,j]))^2/(1+c*abs(GG[,j])),
                     c*lam1*sign(GG[,j])/(1+c*abs(GG[,j]))^2/(1+c*abs(BB[,j])))
        }
        ########################################################################################################
        dif=matrix(0,(t-1)*(1+2*p),t*(1+2*p))
        diag(dif[,-(((t-1)*(1+2*p)+1):(t*(1+2*p)))])=-1
        diag(dif[,-(1:(1+2*p))])=1
        D=cbind(dif,matrix(0,(t-1)*(1+2*p),1+p))
        
        eta=S(D%*%theta/0.0001)
        pp2=lam2*t(D)%*%eta
        #########################################################################################################
        return(c(-2*ga)+c(pp1)+c(pp2))
      }
      ###############################################################################################################
      par=optim(old,fn=obj,gr=gra,method="BFGS",control=list(trace=0,maxit=10000))$par
      if((sum(abs(old-par))<0.000001) | (sss==control)){break}
      sss=sss+1
    }
  }else if(patt=="T5"){ #missingness structure (T5)
    data_ave=matrix(,nrow=0,ncol=p+5)
    for (i in 1:length(data)) {
      
      sub=Reduce("+",data[[i]])/length(data[[i]])
      data_ave=rbind(data_ave,sub)
    }
    ###############################################################################################
    S=function(u){ -(-u>1)+(u>1)+u*(-1<=u & u<=1) }
    
    scad=function(u,lam=lam1){
      lam*u*(u<=lam)+
        (a*lam*u-0.5*(u^2+lam^2))/(a-1)*((lam<u)&(u<=a*lam))+
        0.5*(a+1)*lam^2*(u>a*lam)
    }
    
    scadd=function(u,lam=lam1){
      lam*((u<=lam)+(a*lam-u)*(a*lam>u)/(a-1)/lam*(u>lam))
    }
    #initial########################################################################################
    AA=rep(0,t)
    BB=matrix(0,p,t)
    GG=matrix(0,p,t)
    DD=0
    TT=rep(0,p)
    
    for (j in 1:t) {
      
      sub=data_ave[which(data_ave[,p+4]==j),]
      
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
    par=c(c(rbind(AA,BB,GG)),DD,TT)
    ########################################################################
    sss=1;control=1
    while(1){
      
      old=par
      moment=function(theta,kkk){
        
        fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
        AA=fen[1,]
        BB=fen[2:(1+p),]
        GG=fen[(p+2):(1+2*p),]
        DD=theta[(1+2*p)*t+1]
        TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
        
        data=data[[kkk]]
        n=nrow(data[[1]])/t
        ##################################################################
        comb=function(i){
          
          imp=data[[i]]
          x=imp[,c(p+1,1:p)]
          y=imp[,p+2]
          
          e=cbind(x,1)*matrix(rep( y-rowSums(x*cbind(rep(AA,n),matrix(rep(BB,n),ncol=p,byrow=T)))-rep(DD,n*t) ,p+2),ncol=p+2)
          e1=matrix(c(t(e[,1:(p+1)])),nrow=n,byrow=T)
          e2=rowsum(e[,p+2],rep(1:n,each=t))
          return(cbind(e1,e2))
        }
        e12=Reduce(cbind,lapply(1:length(data),comb))
        e1=e12[,-seq((p+1)*t+1,by=(p+1)*t+1,length.out=length(data))]
        e2=e12[,seq((p+1)*t+1,by=(p+1)*t+1,length.out=length(data))]
        
        if(kkk %in% c(1,27,28,29,30,31)) { e12=cbind(e1,e2) 
        }else if (kkk==2) { len=sort(sequence(nvec=rep(length(data)-1,4*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+4))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) 
        }else if (kkk==3) { len=sort(sequence(nvec=rep(length(data)-1,4*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+3)),((p+1)*(t+4)+1):((p+1)*(t+5))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) 
        }else if (kkk==4) { len=sort(sequence(nvec=rep(length(data)-1,4*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+2)),((p+1)*(t+3)+1):((p+1)*(t+5))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2))
        }else if (kkk==5) { len=sort(sequence(nvec=rep(length(data)-1,4*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+2)+1):((p+1)*(t+5))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) 
        }else if (kkk==6) { len=sort(sequence(nvec=rep(length(data)-1,4*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+5))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2))
        }else if (kkk==7) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+3))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2))
        }else if (kkk==8) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+2)),((p+1)*(t+3)+1):((p+1)*(t+4))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) 
        }else if (kkk==9) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+2)),((p+1)*(t+4)+1):((p+1)*(t+5))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2))
        }else if (kkk==10) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+2)+1):((p+1)*(t+4))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2))
        }else if (kkk==11) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+2)+1):((p+1)*(t+3)),((p+1)*(t+4)+1):((p+1)*(t+5))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) 
        }else if (kkk==12) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+3)+1):((p+1)*(t+5))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2))
        }else if (kkk==13) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+4))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2))
        }else if (kkk==14) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+3)),((p+1)*(t+4)+1):((p+1)*(t+5))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2))
        }else if (kkk==15) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+2)),((p+1)*(t+3)+1):((p+1)*(t+5))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2))
        }else if (kkk==16) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*(t+2)+1):((p+1)*(t+5))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2))
        }else if (kkk==17) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+2))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2))
        }else if (kkk==18) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+2)+1):((p+1)*(t+3))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) 
        }else if (kkk==19) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+3)+1):((p+1)*(t+4))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) 
        }else if (kkk==20) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+4)+1):((p+1)*(t+5))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2))
        }else if (kkk==21) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+3))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2))
        }else if (kkk==22) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+2)),((p+1)*(t+3)+1):((p+1)*(t+4))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2))
        }else if (kkk==23) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+2)),((p+1)*(t+4)+1):((p+1)*(t+5))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2))
        }else if (kkk==24) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+2)+1):((p+1)*(t+4))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2))
        }else if (kkk==25) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+2)+1):((p+1)*(t+3)),((p+1)*(t+4)+1):((p+1)*(t+5))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2))
        }else if (kkk==26) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+3)+1):((p+1)*(t+5))),by=(p+1)*t))
        e12=cbind(e1[,-len],rowMeans(e2)) }
        E12=e12
        #############################################################################################
        comb=function(i){
          
          imp=data[[i]]
          x=imp[,p+1]
          y=imp[,1:p]
          
          e=y-matrix(rep(x,p),ncol=p)*matrix(rep(GG,n),ncol=p,byrow=T)-matrix(rep(TT,n*t),ncol=p,byrow=T)
          ex=matrix(rep(x,p),ncol=p)*e
          e1=matrix(c(t(ex)),nrow=n,byrow=T)
          e2=rowsum(e,rep(1:n,each=t))
          return(cbind(e1,e2))
        }
        e12=Reduce(cbind,lapply(1:length(data),comb))
        e1=e12[,-sort(sequence(nvec=rep(length(data),p),from=c((p*t+1):(p*(t+1))),by=p*(t+1)))]
        e2=e12[,sort(sequence(nvec=rep(length(data),p),from=c((p*t+1):(p*(t+1))),by=p*(t+1)))]
        
        if(kkk %in% c(1,27,28,29,30,31)) { e12=cbind(e1,e2) 
        }else if (kkk==2) { len=sort(sequence(nvec=rep(length(data)-1,4*p),from=c((p*t+1):(p*(t+4))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==3) { len=sort(sequence(nvec=rep(length(data)-1,4*p),from=c((p*t+1):(p*(t+3)),(p*(t+4)+1):(p*(t+5))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==4) { len=sort(sequence(nvec=rep(length(data)-1,4*p),from=c((p*t+1):(p*(t+2)),(p*(t+3)+1):(p*(t+5))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data))
        }else if (kkk==5) { len=sort(sequence(nvec=rep(length(data)-1,4*p),from=c((p*t+1):(p*(t+1)),(p*(t+2)+1):(p*(t+5))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==6) { len=sort(sequence(nvec=rep(length(data)-1,4*p),from=c((p*(t+1)+1):(p*(t+5))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data))
        }else if (kkk==7) { len=sort(sequence(nvec=rep(length(data)-1,3*p),from=c((p*t+1):(p*(t+3))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data))
        }else if (kkk==8) { len=sort(sequence(nvec=rep(length(data)-1,3*p),from=c((p*t+1):(p*(t+2)),(p*(t+3)+1):(p*(t+4))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==9) { len=sort(sequence(nvec=rep(length(data)-1,3*p),from=c((p*t+1):(p*(t+2)),(p*(t+4)+1):(p*(t+5))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data))
        }else if (kkk==10) { len=sort(sequence(nvec=rep(length(data)-1,3*p),from=c((p*t+1):(p*(t+1)),(p*(t+2)+1):(p*(t+4))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data))
        }else if (kkk==11) { len=sort(sequence(nvec=rep(length(data)-1,3*p),from=c((p*t+1):(p*(t+1)),(p*(t+2)+1):(p*(t+3)),(p*(t+4)+1):(p*(t+5))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==12) { len=sort(sequence(nvec=rep(length(data)-1,3*p),from=c((p*t+1):(p*(t+1)),(p*(t+3)+1):(p*(t+5))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data))
        }else if (kkk==13) { len=sort(sequence(nvec=rep(length(data)-1,3*p),from=c((p*(t+1)+1):(p*(t+4))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data))
        }else if (kkk==14) { len=sort(sequence(nvec=rep(length(data)-1,3*p),from=c((p*(t+1)+1):(p*(t+3)),(p*(t+4)+1):(p*(t+5))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data))
        }else if (kkk==15) { len=sort(sequence(nvec=rep(length(data)-1,3*p),from=c((p*(t+1)+1):(p*(t+2)),(p*(t+3)+1):(p*(t+5))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data))
        }else if (kkk==16) { len=sort(sequence(nvec=rep(length(data)-1,3*p),from=c((p*(t+2)+1):(p*(t+5))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data))
        }else if (kkk==17) { len=sort(sequence(nvec=rep(length(data)-1,2*p),from=c((p*t+1):(p*(t+2))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data))
        }else if (kkk==18) { len=sort(sequence(nvec=rep(length(data)-1,2*p),from=c((p*t+1):(p*(t+1)),(p*(t+2)+1):(p*(t+3))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==19) { len=sort(sequence(nvec=rep(length(data)-1,2*p),from=c((p*t+1):(p*(t+1)),(p*(t+3)+1):(p*(t+4))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) 
        }else if (kkk==20) { len=sort(sequence(nvec=rep(length(data)-1,2*p),from=c((p*t+1):(p*(t+1)),(p*(t+4)+1):(p*(t+5))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data))
        }else if (kkk==21) { len=sort(sequence(nvec=rep(length(data)-1,2*p),from=c((p*(t+1)+1):(p*(t+3))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data))
        }else if (kkk==22) { len=sort(sequence(nvec=rep(length(data)-1,2*p),from=c((p*(t+1)+1):(p*(t+2)),(p*(t+3)+1):(p*(t+4))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data))
        }else if (kkk==23) { len=sort(sequence(nvec=rep(length(data)-1,2*p),from=c((p*(t+1)+1):(p*(t+2)),(p*(t+4)+1):(p*(t+5))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data))
        }else if (kkk==24) { len=sort(sequence(nvec=rep(length(data)-1,2*p),from=c((p*(t+2)+1):(p*(t+4))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data))
        }else if (kkk==25) { len=sort(sequence(nvec=rep(length(data)-1,2*p),from=c((p*(t+2)+1):(p*(t+3)),(p*(t+4)+1):(p*(t+5))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data))
        }else if (kkk==26) { len=sort(sequence(nvec=rep(length(data)-1,2*p),from=c((p*(t+3)+1):(p*(t+5))),by=p*t))
        e12=cbind(e1[,-len],t(rowsum(t(e2),rep(1:p,length(data))))/length(data)) }
        ##################################################################################################
        mmm=function(j){ return( e12[,seq(j,by=p,length.out=ncol(e12)/p)] ) }
        out=lapply(1:p,mmm)
        out[[p+1]]=E12
        return(out)
      }
      ###################################################################################################
      derivative=function(theta,kkk){
        
        fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
        AA=fen[1,]
        BB=fen[2:(1+p),]
        GG=fen[(p+2):(1+2*p),]
        DD=theta[(1+2*p)*t+1]
        TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
        
        data=data[[kkk]]
        n=nrow(data[[1]])/t
        ###################################################################
        comb=function(i){
          
          imp=data[[i]][,c(p+1,1:p)]
          x=matrix(rep(imp,t),nrow=n*t)
          y=t(matrix(rep(bdiag(rep(list(rep(1,p+1)),t)),n),ncol=n*t))
          u=x*y
          u1=cbind(u,1)
          
          return( rbind(t(u)%*%u1,colSums(u1))/n )
        }
        d12=Reduce(rbind,lapply(1:length(data),comb))
        d1=d12[-seq((p+1)*t+1,by=(p+1)*t+1,length.out=length(data)),]
        d2=d12[seq((p+1)*t+1,by=(p+1)*t+1,length.out=length(data)),]
        
        if(kkk %in% c(1,27,28,29,30,31)) { d12=rbind(d1,d2) 
        }else if (kkk==2) { len=sort(sequence(nvec=rep(length(data)-1,4*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+4))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) 
        }else if (kkk==3) { len=sort(sequence(nvec=rep(length(data)-1,4*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+3)),((p+1)*(t+4)+1):((p+1)*(t+5))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) 
        }else if (kkk==4) { len=sort(sequence(nvec=rep(length(data)-1,4*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+2)),((p+1)*(t+3)+1):((p+1)*(t+5))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2))
        }else if (kkk==5) { len=sort(sequence(nvec=rep(length(data)-1,4*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+2)+1):((p+1)*(t+5))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) 
        }else if (kkk==6) { len=sort(sequence(nvec=rep(length(data)-1,4*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+5))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2))
        }else if (kkk==7) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+3))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2))
        }else if (kkk==8) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+2)),((p+1)*(t+3)+1):((p+1)*(t+4))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) 
        }else if (kkk==9) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+2)),((p+1)*(t+4)+1):((p+1)*(t+5))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2))
        }else if (kkk==10) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+2)+1):((p+1)*(t+4))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2))
        }else if (kkk==11) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+2)+1):((p+1)*(t+3)),((p+1)*(t+4)+1):((p+1)*(t+5))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) 
        }else if (kkk==12) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+3)+1):((p+1)*(t+5))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2))
        }else if (kkk==13) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+4))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2))
        }else if (kkk==14) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+3)),((p+1)*(t+4)+1):((p+1)*(t+5))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2))
        }else if (kkk==15) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+2)),((p+1)*(t+3)+1):((p+1)*(t+5))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2))
        }else if (kkk==16) { len=sort(sequence(nvec=rep(length(data)-1,3*(p+1)),from=c(((p+1)*(t+2)+1):((p+1)*(t+5))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2))
        }else if (kkk==17) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+2))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2))
        }else if (kkk==18) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+2)+1):((p+1)*(t+3))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) 
        }else if (kkk==19) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+3)+1):((p+1)*(t+4))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) 
        }else if (kkk==20) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*t+1):((p+1)*(t+1)),((p+1)*(t+4)+1):((p+1)*(t+5))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2))
        }else if (kkk==21) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+3))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2))
        }else if (kkk==22) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+2)),((p+1)*(t+3)+1):((p+1)*(t+4))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2))
        }else if (kkk==23) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+1)+1):((p+1)*(t+2)),((p+1)*(t+4)+1):((p+1)*(t+5))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2))
        }else if (kkk==24) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+2)+1):((p+1)*(t+4))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2))
        }else if (kkk==25) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+2)+1):((p+1)*(t+3)),((p+1)*(t+4)+1):((p+1)*(t+5))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2))
        }else if (kkk==26) { len=sort(sequence(nvec=rep(length(data)-1,2*(p+1)),from=c(((p+1)*(t+3)+1):((p+1)*(t+5))),by=(p+1)*t))
        d12=rbind(d1[-len,],colMeans(d2)) }
        
        D12=matrix(0,nrow(d12),(1+2*p)*t+1+p)
        len=c(((1+2*p)*0+1):((1+2*p)*0+1+p),((1+2*p)*1+1):((1+2*p)*1+1+p),((1+2*p)*2+1):((1+2*p)*2+1+p),((1+2*p)*3+1):((1+2*p)*3+1+p),((1+2*p)*4+1):((1+2*p)*4+1+p),(1+2*p)*t+1)
        D12[,len]=d12
        ########################################################################################################
        imp=data[[1]][,p+1]
        x=matrix(rep(imp,t),nrow=n*t)
        y=t(matrix(rep(bdiag(rep(list(rep(1,1)),t)),n),ncol=n*t))
        u=x*y
        u1=cbind(u,1)
        d1=t(u)%*%u1/n
        d2=colSums(u1)/n
        
        if(kkk %in% c(1,27,28,29,30,31)) { d12=rbind(d1,d2) 
        }else if (kkk==2) { d12=rbind(d1,matrix(rep(d1[5,],length(data)-1),ncol=ncol(d1),byrow=T),d2) 
        }else if (kkk==3) { d12=rbind(d1,matrix(rep(d1[4,],length(data)-1),ncol=ncol(d1),byrow=T),d2) 
        }else if (kkk==4) { d12=rbind(d1,matrix(rep(d1[3,],length(data)-1),ncol=ncol(d1),byrow=T),d2) 
        }else if (kkk==5) { d12=rbind(d1,matrix(rep(d1[2,],length(data)-1),ncol=ncol(d1),byrow=T),d2) 
        }else if (kkk==6) { d12=rbind(d1,matrix(rep(d1[1,],length(data)-1),ncol=ncol(d1),byrow=T),d2) 
        
        }else if (kkk==7) { d12=rbind(d1,matrix(rep(t(d1[c(4,5),]),length(data)-1),ncol=ncol(d1),byrow=T),d2) 
        }else if (kkk==8) { d12=rbind(d1,matrix(rep(t(d1[c(3,5),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==9) { d12=rbind(d1,matrix(rep(t(d1[c(3,4),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==10){ d12=rbind(d1,matrix(rep(t(d1[c(2,5),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==11){ d12=rbind(d1,matrix(rep(t(d1[c(2,4),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==12){ d12=rbind(d1,matrix(rep(t(d1[c(2,3),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==13){ d12=rbind(d1,matrix(rep(t(d1[c(1,5),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==14){ d12=rbind(d1,matrix(rep(t(d1[c(1,4),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==15){ d12=rbind(d1,matrix(rep(t(d1[c(1,3),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==16){ d12=rbind(d1,matrix(rep(t(d1[c(1,2),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==17){ d12=rbind(d1,matrix(rep(t(d1[c(3,4,5),]),length(data)-1),ncol=ncol(d1),byrow=T),d2) 
        }else if (kkk==18){ d12=rbind(d1,matrix(rep(t(d1[c(2,4,5),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==19){ d12=rbind(d1,matrix(rep(t(d1[c(2,3,5),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==20){ d12=rbind(d1,matrix(rep(t(d1[c(2,3,4),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==21){ d12=rbind(d1,matrix(rep(t(d1[c(1,4,5),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==22){ d12=rbind(d1,matrix(rep(t(d1[c(1,3,5),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==23){ d12=rbind(d1,matrix(rep(t(d1[c(1,3,4),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==24){ d12=rbind(d1,matrix(rep(t(d1[c(1,2,5),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==25){ d12=rbind(d1,matrix(rep(t(d1[c(1,2,4),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)
        }else if (kkk==26){ d12=rbind(d1,matrix(rep(t(d1[c(1,2,3),]),length(data)-1),ncol=ncol(d1),byrow=T),d2)}
        #################################################################################################
        ddd=function(j){
          
          D12=matrix(0,nrow(d12),(1+2*p)*t+1+p)
          len=c(seq(1+p+j,(1+2*p)*(t-1)+1+p+j,length.out=t),(1+2*p)*t+1+j)
          D12[,len]=d12
          return(D12)
        }
        out=lapply(1:p,ddd)
        out[[p+1]]=D12
        return(out)
      }
      #################################################################################################
      pca=function(ef){
        
        n=nrow(ef);d=ncol(ef)
        w=t(ef)%*%ef/n
        
        eg=eigen(w)
        eg1=eg$values
        eg2=eg$vectors
        
        if(abs(max(eg1)/min(eg1))>100000){
          
          tt=sum(eg1>(sum(diag(w))*log(n*d)/(n*d)))
          return(t(eg2[,1:tt]))
        }
        else{ return(diag(d)) }
      }
      ##############################################################################################################
      eo1=moment(old,1);pc1=lapply(eo1,pca)
      eo2=moment(old,2);pc2=lapply(eo2,pca)
      eo3=moment(old,3);pc3=lapply(eo3,pca)
      eo4=moment(old,4);pc4=lapply(eo4,pca)
      eo5=moment(old,5);pc5=lapply(eo5,pca)
      eo6=moment(old,6);pc6=lapply(eo6,pca)
      eo7=moment(old,7);pc7=lapply(eo7,pca)
      eo8=moment(old,8);pc8=lapply(eo8,pca)
      eo9=moment(old,9);pc9=lapply(eo9,pca)
      eo10=moment(old,10);pc10=lapply(eo10,pca)
      eo11=moment(old,11);pc11=lapply(eo11,pca)
      eo12=moment(old,12);pc12=lapply(eo12,pca)
      eo13=moment(old,13);pc13=lapply(eo13,pca)
      eo14=moment(old,14);pc14=lapply(eo14,pca)
      eo15=moment(old,15);pc15=lapply(eo15,pca)
      eo16=moment(old,16);pc16=lapply(eo16,pca)
      eo17=moment(old,17);pc17=lapply(eo17,pca)
      eo18=moment(old,18);pc18=lapply(eo18,pca)
      eo19=moment(old,19);pc19=lapply(eo19,pca)
      eo20=moment(old,20);pc20=lapply(eo20,pca)
      eo21=moment(old,21);pc21=lapply(eo21,pca)
      eo22=moment(old,22);pc22=lapply(eo22,pca)
      eo23=moment(old,23);pc23=lapply(eo23,pca)
      eo24=moment(old,24);pc24=lapply(eo24,pca)
      eo25=moment(old,25);pc25=lapply(eo25,pca)
      eo26=moment(old,26);pc26=lapply(eo26,pca)
      eo27=moment(old,27);pc27=lapply(eo27,pca)
      eo28=moment(old,28);pc28=lapply(eo28,pca)
      eo29=moment(old,29);pc29=lapply(eo29,pca)
      eo30=moment(old,30);pc30=lapply(eo30,pca)
      eo31=moment(old,31);pc31=lapply(eo31,pca)
      ##############################################################################################################
      obj=function(theta){
        
        ef1=moment(theta,1)
        ef2=moment(theta,2)
        ef3=moment(theta,3)
        ef4=moment(theta,4)
        ef5=moment(theta,5)
        ef6=moment(theta,6)
        ef7=moment(theta,7)
        ef8=moment(theta,8)
        ef9=moment(theta,9)
        ef10=moment(theta,10)
        ef11=moment(theta,11)
        ef12=moment(theta,12)
        ef13=moment(theta,13)
        ef14=moment(theta,14)
        ef15=moment(theta,15)
        ef16=moment(theta,16)
        ef17=moment(theta,17)
        ef18=moment(theta,18)
        ef19=moment(theta,19)
        ef20=moment(theta,20)
        ef21=moment(theta,21)
        ef22=moment(theta,22)
        ef23=moment(theta,23)
        ef24=moment(theta,24)
        ef25=moment(theta,25)
        ef26=moment(theta,26)
        ef27=moment(theta,27)
        ef28=moment(theta,28)
        ef29=moment(theta,29)
        ef30=moment(theta,30)
        ef31=moment(theta,31)
        
        va=function(j){
          return(t(pc1[[j]]%*%as.matrix(colMeans(ef1[[j]])))%*%solve(pc1[[j]]%*%t(eo1[[j]])%*%t(pc1[[j]]%*%t(eo1[[j]]))/nrow(eo1[[j]]))%*%pc1[[j]]%*%as.matrix(colMeans(ef1[[j]]))+
                   t(pc2[[j]]%*%as.matrix(colMeans(ef2[[j]])))%*%solve(pc2[[j]]%*%t(eo2[[j]])%*%t(pc2[[j]]%*%t(eo2[[j]]))/nrow(eo2[[j]]))%*%pc2[[j]]%*%as.matrix(colMeans(ef2[[j]]))+
                   t(pc3[[j]]%*%as.matrix(colMeans(ef3[[j]])))%*%solve(pc3[[j]]%*%t(eo3[[j]])%*%t(pc3[[j]]%*%t(eo3[[j]]))/nrow(eo3[[j]]))%*%pc3[[j]]%*%as.matrix(colMeans(ef3[[j]]))+
                   t(pc4[[j]]%*%as.matrix(colMeans(ef4[[j]])))%*%solve(pc4[[j]]%*%t(eo4[[j]])%*%t(pc4[[j]]%*%t(eo4[[j]]))/nrow(eo4[[j]]))%*%pc4[[j]]%*%as.matrix(colMeans(ef4[[j]]))+
                   t(pc5[[j]]%*%as.matrix(colMeans(ef5[[j]])))%*%solve(pc5[[j]]%*%t(eo5[[j]])%*%t(pc5[[j]]%*%t(eo5[[j]]))/nrow(eo5[[j]]))%*%pc5[[j]]%*%as.matrix(colMeans(ef5[[j]]))+
                   t(pc6[[j]]%*%as.matrix(colMeans(ef6[[j]])))%*%solve(pc6[[j]]%*%t(eo6[[j]])%*%t(pc6[[j]]%*%t(eo6[[j]]))/nrow(eo6[[j]]))%*%pc6[[j]]%*%as.matrix(colMeans(ef6[[j]]))+
                   t(pc7[[j]]%*%as.matrix(colMeans(ef7[[j]])))%*%solve(pc7[[j]]%*%t(eo7[[j]])%*%t(pc7[[j]]%*%t(eo7[[j]]))/nrow(eo7[[j]]))%*%pc7[[j]]%*%as.matrix(colMeans(ef7[[j]]))+
                   t(pc8[[j]]%*%as.matrix(colMeans(ef8[[j]])))%*%solve(pc8[[j]]%*%t(eo8[[j]])%*%t(pc8[[j]]%*%t(eo8[[j]]))/nrow(eo8[[j]]))%*%pc8[[j]]%*%as.matrix(colMeans(ef8[[j]]))+
                   t(pc9[[j]]%*%as.matrix(colMeans(ef9[[j]])))%*%solve(pc9[[j]]%*%t(eo9[[j]])%*%t(pc9[[j]]%*%t(eo9[[j]]))/nrow(eo9[[j]]))%*%pc9[[j]]%*%as.matrix(colMeans(ef9[[j]]))+
                   t(pc10[[j]]%*%as.matrix(colMeans(ef10[[j]])))%*%solve(pc10[[j]]%*%t(eo10[[j]])%*%t(pc10[[j]]%*%t(eo10[[j]]))/nrow(eo10[[j]]))%*%pc10[[j]]%*%as.matrix(colMeans(ef10[[j]]))+
                   t(pc11[[j]]%*%as.matrix(colMeans(ef11[[j]])))%*%solve(pc11[[j]]%*%t(eo11[[j]])%*%t(pc11[[j]]%*%t(eo11[[j]]))/nrow(eo11[[j]]))%*%pc11[[j]]%*%as.matrix(colMeans(ef11[[j]]))+
                   t(pc12[[j]]%*%as.matrix(colMeans(ef12[[j]])))%*%solve(pc12[[j]]%*%t(eo12[[j]])%*%t(pc12[[j]]%*%t(eo12[[j]]))/nrow(eo12[[j]]))%*%pc12[[j]]%*%as.matrix(colMeans(ef12[[j]]))+
                   t(pc13[[j]]%*%as.matrix(colMeans(ef13[[j]])))%*%solve(pc13[[j]]%*%t(eo13[[j]])%*%t(pc13[[j]]%*%t(eo13[[j]]))/nrow(eo13[[j]]))%*%pc13[[j]]%*%as.matrix(colMeans(ef13[[j]]))+
                   t(pc14[[j]]%*%as.matrix(colMeans(ef14[[j]])))%*%solve(pc14[[j]]%*%t(eo14[[j]])%*%t(pc14[[j]]%*%t(eo14[[j]]))/nrow(eo14[[j]]))%*%pc14[[j]]%*%as.matrix(colMeans(ef14[[j]]))+
                   t(pc15[[j]]%*%as.matrix(colMeans(ef15[[j]])))%*%solve(pc15[[j]]%*%t(eo15[[j]])%*%t(pc15[[j]]%*%t(eo15[[j]]))/nrow(eo15[[j]]))%*%pc15[[j]]%*%as.matrix(colMeans(ef15[[j]]))+
                   t(pc16[[j]]%*%as.matrix(colMeans(ef16[[j]])))%*%solve(pc16[[j]]%*%t(eo16[[j]])%*%t(pc16[[j]]%*%t(eo16[[j]]))/nrow(eo16[[j]]))%*%pc16[[j]]%*%as.matrix(colMeans(ef16[[j]]))+
                   t(pc17[[j]]%*%as.matrix(colMeans(ef17[[j]])))%*%solve(pc17[[j]]%*%t(eo17[[j]])%*%t(pc17[[j]]%*%t(eo17[[j]]))/nrow(eo17[[j]]))%*%pc17[[j]]%*%as.matrix(colMeans(ef17[[j]]))+
                   t(pc18[[j]]%*%as.matrix(colMeans(ef18[[j]])))%*%solve(pc18[[j]]%*%t(eo18[[j]])%*%t(pc18[[j]]%*%t(eo18[[j]]))/nrow(eo18[[j]]))%*%pc18[[j]]%*%as.matrix(colMeans(ef18[[j]]))+
                   t(pc19[[j]]%*%as.matrix(colMeans(ef19[[j]])))%*%solve(pc19[[j]]%*%t(eo19[[j]])%*%t(pc19[[j]]%*%t(eo19[[j]]))/nrow(eo19[[j]]))%*%pc19[[j]]%*%as.matrix(colMeans(ef19[[j]]))+
                   t(pc20[[j]]%*%as.matrix(colMeans(ef20[[j]])))%*%solve(pc20[[j]]%*%t(eo20[[j]])%*%t(pc20[[j]]%*%t(eo20[[j]]))/nrow(eo20[[j]]))%*%pc20[[j]]%*%as.matrix(colMeans(ef20[[j]]))+
                   t(pc21[[j]]%*%as.matrix(colMeans(ef21[[j]])))%*%solve(pc21[[j]]%*%t(eo21[[j]])%*%t(pc21[[j]]%*%t(eo21[[j]]))/nrow(eo21[[j]]))%*%pc21[[j]]%*%as.matrix(colMeans(ef21[[j]]))+
                   t(pc22[[j]]%*%as.matrix(colMeans(ef22[[j]])))%*%solve(pc22[[j]]%*%t(eo22[[j]])%*%t(pc22[[j]]%*%t(eo22[[j]]))/nrow(eo22[[j]]))%*%pc22[[j]]%*%as.matrix(colMeans(ef22[[j]]))+
                   t(pc23[[j]]%*%as.matrix(colMeans(ef23[[j]])))%*%solve(pc23[[j]]%*%t(eo23[[j]])%*%t(pc23[[j]]%*%t(eo23[[j]]))/nrow(eo23[[j]]))%*%pc23[[j]]%*%as.matrix(colMeans(ef23[[j]]))+
                   t(pc24[[j]]%*%as.matrix(colMeans(ef24[[j]])))%*%solve(pc24[[j]]%*%t(eo24[[j]])%*%t(pc24[[j]]%*%t(eo24[[j]]))/nrow(eo24[[j]]))%*%pc24[[j]]%*%as.matrix(colMeans(ef24[[j]]))+
                   t(pc25[[j]]%*%as.matrix(colMeans(ef25[[j]])))%*%solve(pc25[[j]]%*%t(eo25[[j]])%*%t(pc25[[j]]%*%t(eo25[[j]]))/nrow(eo25[[j]]))%*%pc25[[j]]%*%as.matrix(colMeans(ef25[[j]]))+
                   t(pc26[[j]]%*%as.matrix(colMeans(ef26[[j]])))%*%solve(pc26[[j]]%*%t(eo26[[j]])%*%t(pc26[[j]]%*%t(eo26[[j]]))/nrow(eo26[[j]]))%*%pc26[[j]]%*%as.matrix(colMeans(ef26[[j]]))+
                   t(pc27[[j]]%*%as.matrix(colMeans(ef27[[j]])))%*%solve(pc27[[j]]%*%t(eo27[[j]])%*%t(pc27[[j]]%*%t(eo27[[j]]))/nrow(eo27[[j]]))%*%pc27[[j]]%*%as.matrix(colMeans(ef27[[j]]))+
                   t(pc28[[j]]%*%as.matrix(colMeans(ef28[[j]])))%*%solve(pc28[[j]]%*%t(eo28[[j]])%*%t(pc28[[j]]%*%t(eo28[[j]]))/nrow(eo28[[j]]))%*%pc28[[j]]%*%as.matrix(colMeans(ef28[[j]]))+
                   t(pc29[[j]]%*%as.matrix(colMeans(ef29[[j]])))%*%solve(pc29[[j]]%*%t(eo29[[j]])%*%t(pc29[[j]]%*%t(eo29[[j]]))/nrow(eo29[[j]]))%*%pc29[[j]]%*%as.matrix(colMeans(ef29[[j]]))+
                   t(pc30[[j]]%*%as.matrix(colMeans(ef30[[j]])))%*%solve(pc30[[j]]%*%t(eo30[[j]])%*%t(pc30[[j]]%*%t(eo30[[j]]))/nrow(eo30[[j]]))%*%pc30[[j]]%*%as.matrix(colMeans(ef30[[j]]))+
                   t(pc31[[j]]%*%as.matrix(colMeans(ef31[[j]])))%*%solve(pc31[[j]]%*%t(eo31[[j]])%*%t(pc31[[j]]%*%t(eo31[[j]]))/nrow(eo31[[j]]))%*%pc31[[j]]%*%as.matrix(colMeans(ef31[[j]])))
        }
        va=Reduce("+",lapply(1:(p+1),va))
        ########################################################################################
        fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
        AA=fen[1,]
        BB=fen[2:(1+p),]
        GG=fen[(p+2):(1+2*p),]
        DD=theta[(1+2*p)*t+1]
        TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
        ######################################################################################
        p1=sum(scad(abs(AA)))+lam1*sum(1-1/(1+c*abs(BB))/(1+c*abs(GG)))
        ########################################################
        dif=matrix(0,(t-1)*(1+2*p),t*(1+2*p))
        diag(dif[,-(((t-1)*(1+2*p)+1):(t*(1+2*p)))])=-1
        diag(dif[,-(1:(1+2*p))])=1
        D=cbind(dif,matrix(0,(t-1)*(1+2*p),1+p))
        
        eta=S(D%*%theta/0.0001)
        p2=lam2*(t(eta)%*%D%*%theta-0.0001/2*sum(eta^2))
        ########################################################################################
        return(va+p1+p2)
      }
      ##############################################################################################################
      gra=function(theta){
        
        ef1=moment(theta,1);gf1=derivative(theta,1)
        ef2=moment(theta,2);gf2=derivative(theta,2)
        ef3=moment(theta,3);gf3=derivative(theta,3)
        ef4=moment(theta,4);gf4=derivative(theta,4)
        ef5=moment(theta,5);gf5=derivative(theta,5)
        ef6=moment(theta,6);gf6=derivative(theta,6)
        ef7=moment(theta,7);gf7=derivative(theta,7)
        ef8=moment(theta,8);gf8=derivative(theta,8)
        ef9=moment(theta,9);gf9=derivative(theta,9)
        ef10=moment(theta,10);gf10=derivative(theta,10)
        ef11=moment(theta,11);gf11=derivative(theta,11)
        ef12=moment(theta,12);gf12=derivative(theta,12)
        ef13=moment(theta,13);gf13=derivative(theta,13)
        ef14=moment(theta,14);gf14=derivative(theta,14)
        ef15=moment(theta,15);gf15=derivative(theta,15)
        ef16=moment(theta,16);gf16=derivative(theta,16)
        ef17=moment(theta,17);gf17=derivative(theta,17)
        ef18=moment(theta,18);gf18=derivative(theta,18)
        ef19=moment(theta,19);gf19=derivative(theta,19)
        ef20=moment(theta,20);gf20=derivative(theta,20)
        ef21=moment(theta,21);gf21=derivative(theta,21)
        ef22=moment(theta,22);gf22=derivative(theta,22)
        ef23=moment(theta,23);gf23=derivative(theta,23)
        ef24=moment(theta,24);gf24=derivative(theta,24)
        ef25=moment(theta,25);gf25=derivative(theta,25)
        ef26=moment(theta,26);gf26=derivative(theta,26)
        ef27=moment(theta,27);gf27=derivative(theta,27)
        ef28=moment(theta,28);gf28=derivative(theta,28)
        ef29=moment(theta,29);gf29=derivative(theta,29)
        ef30=moment(theta,30);gf30=derivative(theta,30)
        ef31=moment(theta,31);gf31=derivative(theta,31)
        
        ga=function(j){
          return(t(pc1[[j]]%*%as.matrix(colMeans(ef1[[j]])))%*%solve(pc1[[j]]%*%t(eo1[[j]])%*%t(pc1[[j]]%*%t(eo1[[j]]))/nrow(eo1[[j]]))%*%pc1[[j]]%*%gf1[[j]]+
                   t(pc2[[j]]%*%as.matrix(colMeans(ef2[[j]])))%*%solve(pc2[[j]]%*%t(eo2[[j]])%*%t(pc2[[j]]%*%t(eo2[[j]]))/nrow(eo2[[j]]))%*%pc2[[j]]%*%gf2[[j]]+
                   t(pc3[[j]]%*%as.matrix(colMeans(ef3[[j]])))%*%solve(pc3[[j]]%*%t(eo3[[j]])%*%t(pc3[[j]]%*%t(eo3[[j]]))/nrow(eo3[[j]]))%*%pc3[[j]]%*%gf3[[j]]+
                   t(pc4[[j]]%*%as.matrix(colMeans(ef4[[j]])))%*%solve(pc4[[j]]%*%t(eo4[[j]])%*%t(pc4[[j]]%*%t(eo4[[j]]))/nrow(eo4[[j]]))%*%pc4[[j]]%*%gf4[[j]]+
                   t(pc5[[j]]%*%as.matrix(colMeans(ef5[[j]])))%*%solve(pc5[[j]]%*%t(eo5[[j]])%*%t(pc5[[j]]%*%t(eo5[[j]]))/nrow(eo5[[j]]))%*%pc5[[j]]%*%gf5[[j]]+
                   t(pc6[[j]]%*%as.matrix(colMeans(ef6[[j]])))%*%solve(pc6[[j]]%*%t(eo6[[j]])%*%t(pc6[[j]]%*%t(eo6[[j]]))/nrow(eo6[[j]]))%*%pc6[[j]]%*%gf6[[j]]+
                   t(pc7[[j]]%*%as.matrix(colMeans(ef7[[j]])))%*%solve(pc7[[j]]%*%t(eo7[[j]])%*%t(pc7[[j]]%*%t(eo7[[j]]))/nrow(eo7[[j]]))%*%pc7[[j]]%*%gf7[[j]]+
                   t(pc8[[j]]%*%as.matrix(colMeans(ef8[[j]])))%*%solve(pc8[[j]]%*%t(eo8[[j]])%*%t(pc8[[j]]%*%t(eo8[[j]]))/nrow(eo8[[j]]))%*%pc8[[j]]%*%gf8[[j]]+
                   t(pc9[[j]]%*%as.matrix(colMeans(ef9[[j]])))%*%solve(pc9[[j]]%*%t(eo9[[j]])%*%t(pc9[[j]]%*%t(eo9[[j]]))/nrow(eo9[[j]]))%*%pc9[[j]]%*%gf9[[j]]+
                   t(pc10[[j]]%*%as.matrix(colMeans(ef10[[j]])))%*%solve(pc10[[j]]%*%t(eo10[[j]])%*%t(pc10[[j]]%*%t(eo10[[j]]))/nrow(eo10[[j]]))%*%pc10[[j]]%*%gf10[[j]]+
                   t(pc11[[j]]%*%as.matrix(colMeans(ef11[[j]])))%*%solve(pc11[[j]]%*%t(eo11[[j]])%*%t(pc11[[j]]%*%t(eo11[[j]]))/nrow(eo11[[j]]))%*%pc11[[j]]%*%gf11[[j]]+
                   t(pc12[[j]]%*%as.matrix(colMeans(ef12[[j]])))%*%solve(pc12[[j]]%*%t(eo12[[j]])%*%t(pc12[[j]]%*%t(eo12[[j]]))/nrow(eo12[[j]]))%*%pc12[[j]]%*%gf12[[j]]+
                   t(pc13[[j]]%*%as.matrix(colMeans(ef13[[j]])))%*%solve(pc13[[j]]%*%t(eo13[[j]])%*%t(pc13[[j]]%*%t(eo13[[j]]))/nrow(eo13[[j]]))%*%pc13[[j]]%*%gf13[[j]]+
                   t(pc14[[j]]%*%as.matrix(colMeans(ef14[[j]])))%*%solve(pc14[[j]]%*%t(eo14[[j]])%*%t(pc14[[j]]%*%t(eo14[[j]]))/nrow(eo14[[j]]))%*%pc14[[j]]%*%gf14[[j]]+
                   t(pc15[[j]]%*%as.matrix(colMeans(ef15[[j]])))%*%solve(pc15[[j]]%*%t(eo15[[j]])%*%t(pc15[[j]]%*%t(eo15[[j]]))/nrow(eo15[[j]]))%*%pc15[[j]]%*%gf15[[j]]+
                   t(pc16[[j]]%*%as.matrix(colMeans(ef16[[j]])))%*%solve(pc16[[j]]%*%t(eo16[[j]])%*%t(pc16[[j]]%*%t(eo16[[j]]))/nrow(eo16[[j]]))%*%pc16[[j]]%*%gf16[[j]]+
                   t(pc17[[j]]%*%as.matrix(colMeans(ef17[[j]])))%*%solve(pc17[[j]]%*%t(eo17[[j]])%*%t(pc17[[j]]%*%t(eo17[[j]]))/nrow(eo17[[j]]))%*%pc17[[j]]%*%gf17[[j]]+
                   t(pc18[[j]]%*%as.matrix(colMeans(ef18[[j]])))%*%solve(pc18[[j]]%*%t(eo18[[j]])%*%t(pc18[[j]]%*%t(eo18[[j]]))/nrow(eo18[[j]]))%*%pc18[[j]]%*%gf18[[j]]+
                   t(pc19[[j]]%*%as.matrix(colMeans(ef19[[j]])))%*%solve(pc19[[j]]%*%t(eo19[[j]])%*%t(pc19[[j]]%*%t(eo19[[j]]))/nrow(eo19[[j]]))%*%pc19[[j]]%*%gf19[[j]]+
                   t(pc20[[j]]%*%as.matrix(colMeans(ef20[[j]])))%*%solve(pc20[[j]]%*%t(eo20[[j]])%*%t(pc20[[j]]%*%t(eo20[[j]]))/nrow(eo20[[j]]))%*%pc20[[j]]%*%gf20[[j]]+
                   t(pc21[[j]]%*%as.matrix(colMeans(ef21[[j]])))%*%solve(pc21[[j]]%*%t(eo21[[j]])%*%t(pc21[[j]]%*%t(eo21[[j]]))/nrow(eo21[[j]]))%*%pc21[[j]]%*%gf21[[j]]+
                   t(pc22[[j]]%*%as.matrix(colMeans(ef22[[j]])))%*%solve(pc22[[j]]%*%t(eo22[[j]])%*%t(pc22[[j]]%*%t(eo22[[j]]))/nrow(eo22[[j]]))%*%pc22[[j]]%*%gf22[[j]]+
                   t(pc23[[j]]%*%as.matrix(colMeans(ef23[[j]])))%*%solve(pc23[[j]]%*%t(eo23[[j]])%*%t(pc23[[j]]%*%t(eo23[[j]]))/nrow(eo23[[j]]))%*%pc23[[j]]%*%gf23[[j]]+
                   t(pc24[[j]]%*%as.matrix(colMeans(ef24[[j]])))%*%solve(pc24[[j]]%*%t(eo24[[j]])%*%t(pc24[[j]]%*%t(eo24[[j]]))/nrow(eo24[[j]]))%*%pc24[[j]]%*%gf24[[j]]+
                   t(pc25[[j]]%*%as.matrix(colMeans(ef25[[j]])))%*%solve(pc25[[j]]%*%t(eo25[[j]])%*%t(pc25[[j]]%*%t(eo25[[j]]))/nrow(eo25[[j]]))%*%pc25[[j]]%*%gf25[[j]]+
                   t(pc26[[j]]%*%as.matrix(colMeans(ef26[[j]])))%*%solve(pc26[[j]]%*%t(eo26[[j]])%*%t(pc26[[j]]%*%t(eo26[[j]]))/nrow(eo26[[j]]))%*%pc26[[j]]%*%gf26[[j]]+
                   t(pc27[[j]]%*%as.matrix(colMeans(ef27[[j]])))%*%solve(pc27[[j]]%*%t(eo27[[j]])%*%t(pc27[[j]]%*%t(eo27[[j]]))/nrow(eo27[[j]]))%*%pc27[[j]]%*%gf27[[j]]+
                   t(pc28[[j]]%*%as.matrix(colMeans(ef28[[j]])))%*%solve(pc28[[j]]%*%t(eo28[[j]])%*%t(pc28[[j]]%*%t(eo28[[j]]))/nrow(eo28[[j]]))%*%pc28[[j]]%*%gf28[[j]]+
                   t(pc29[[j]]%*%as.matrix(colMeans(ef29[[j]])))%*%solve(pc29[[j]]%*%t(eo29[[j]])%*%t(pc29[[j]]%*%t(eo29[[j]]))/nrow(eo29[[j]]))%*%pc29[[j]]%*%gf29[[j]]+
                   t(pc30[[j]]%*%as.matrix(colMeans(ef30[[j]])))%*%solve(pc30[[j]]%*%t(eo30[[j]])%*%t(pc30[[j]]%*%t(eo30[[j]]))/nrow(eo30[[j]]))%*%pc30[[j]]%*%gf30[[j]]+
                   t(pc31[[j]]%*%as.matrix(colMeans(ef31[[j]])))%*%solve(pc31[[j]]%*%t(eo31[[j]])%*%t(pc31[[j]]%*%t(eo31[[j]]))/nrow(eo31[[j]]))%*%pc31[[j]]%*%gf31[[j]])
        }
        ga=Reduce("+",lapply(1:(p+1),ga))
        #########################################################################################
        fen=matrix(theta[1:((1+2*p)*t)],ncol=t)
        AA=fen[1,]
        BB=fen[2:(1+p),]
        GG=fen[(p+2):(1+2*p),]
        DD=theta[(1+2*p)*t+1]
        TT=theta[((1+2*p)*t+2):((1+2*p)*t+1+p)]
        ##################################################################################
        pp1=rep(0,(1+2*p)*t+1+p)
        for (j in 1:t) {
          
          len=((j-1)*(1+2*p)+1):(j*(1+2*p))
          pp1[len]=c(scadd(abs(AA[j]))*sign(AA[j]),
                     c*lam1*sign(BB[,j])/(1+c*abs(BB[,j]))^2/(1+c*abs(GG[,j])),
                     c*lam1*sign(GG[,j])/(1+c*abs(GG[,j]))^2/(1+c*abs(BB[,j])))
        }
        ########################################################################################################
        dif=matrix(0,(t-1)*(1+2*p),t*(1+2*p))
        diag(dif[,-(((t-1)*(1+2*p)+1):(t*(1+2*p)))])=-1
        diag(dif[,-(1:(1+2*p))])=1
        D=cbind(dif,matrix(0,(t-1)*(1+2*p),1+p))
        
        eta=S(D%*%theta/0.0001)
        pp2=lam2*t(D)%*%eta
        #########################################################################################################
        return(c(-2*ga)+c(pp1)+c(pp2))
      }
      ###############################################################################################################
      par=optim(old,fn=obj,gr=gra,method="BFGS",control=list(trace=0,maxit=10000))$par
      if((sum(abs(old-par))<0.000001) | (sss==control)){break}
      sss=sss+1
    }
  }
  return(round(par,2))
}