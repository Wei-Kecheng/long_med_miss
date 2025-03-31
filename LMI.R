LMI=function(data,p,t){
  
  for (i in 1:15) {
    sub=data[which(data[,ncol(data)]==i),]
    assign(paste0('data',i),sub)
    assign(paste0('n',i),nrow(sub)/t)
  }
  for (i in c('a','b','c','d','e','f','g')) { assign(paste0('data2',i),data2) }
  for (i in c('a','b','c')) { assign(paste0('data3',i),data3) }
  for (i in c('a','b','c')) { assign(paste0('data6',i),data6) }
  for (i in c('a','b','c')) { assign(paste0('data7',i),data7) }
  for (i in c('a','b','c')) { assign(paste0('data9',i),data9) }
  
  data1a=data1
  data4a=data4
  data5a=data5
  data8a=data8
  data10a=data10
  data11a=data11
  data12a=data12
  data13a=data13
  data14a=data14
  data15a=data15
  #impute each dimension of mediators##############################################################################################################
  for (i in 1:p) {
    #learn correlations through regression
    sub=data1;n=n1
    x123=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y4=sub[seq(from=4,by=t,length.out=n),i]
    cv.fit_1234=cv.glmnet(x123,y4)
    #################################################
    sub=rbind(data1,data2);n=n1+n2
    x12=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,3),from=c(1,2,3),by=t)),p+1],nrow=n,byrow=T))
    y3=sub[seq(from=3,by=t,length.out=n),i]
    cv.fit_123=cv.glmnet(x12,y3)
    
    sub=rbind(data1,data3);n=n1+n3
    x12=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y4=sub[seq(from=4,by=t,length.out=n),i]
    cv.fit_124=cv.glmnet(x12,y4)
    
    sub=rbind(data1,data4);n=n1+n4
    x13=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y4=sub[seq(from=4,by=t,length.out=n),i]
    cv.fit_134=cv.glmnet(x13,y4)
    
    sub=rbind(data1,data5);n=n1+n5
    x23=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y4=sub[seq(from=4,by=t,length.out=n),i]
    cv.fit_234=cv.glmnet(x23,y4)
    ##############################################################################################
    sub=rbind(data1,data2,data3,data6);n=n1+n2+n3+n6
    x1=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,2),from=c(1,2),by=t)),p+1],nrow=n,byrow=T))
    y2=sub[seq(from=2,by=t,length.out=n),i]
    cv.fit_12=cv.glmnet(x1,y2)
    
    sub=rbind(data1,data2,data4,data7);n=n1+n2+n4+n7
    x1=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,3),from=c(1,2,3),by=t)),p+1],nrow=n,byrow=T))
    y3=sub[seq(from=3,by=t,length.out=n),i]
    cv.fit_13=cv.glmnet(x1,y3)
    
    sub=rbind(data1,data3,data4,data8);n=n1+n3+n4+n8
    x1=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y4=sub[seq(from=4,by=t,length.out=n),i]
    cv.fit_14=cv.glmnet(x1,y4)
    
    sub=rbind(data1,data2,data5,data9);n=n1+n2+n5+n9
    x2=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,3),from=c(1,2,3),by=t)),p+1],nrow=n,byrow=T))
    y3=sub[seq(from=3,by=t,length.out=n),i]
    cv.fit_23=cv.glmnet(x2,y3)
    
    sub=rbind(data1,data3,data5,data10);n=n1+n3+n5+n10
    x2=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y4=sub[seq(from=4,by=t,length.out=n),i]
    cv.fit_24=cv.glmnet(x2,y4)
    
    sub=rbind(data1,data4,data5,data11);n=n1+n4+n5+n11
    x3=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y4=sub[seq(from=4,by=t,length.out=n),i]
    cv.fit_34=cv.glmnet(x3,y4)
    ###################################################################################################
    sub=rbind(data1,data2,data3,data4,data6,data7,data8,data12);n=n1+n2+n3+n4+n6+n7+n8+n12
    x=sub[seq(from=1,by=t,length.out=n),p+1]
    y1=sub[seq(from=1,by=t,length.out=n),i]
    cv.fit_1=lm(y1~x)
    
    sub=rbind(data1,data2,data3,data5,data6,data9,data10,data13);n=n1+n2+n3+n5+n6+n9+n10+n13
    x=matrix(sub[sort(sequence(nvec=rep(n,2),from=c(1,2),by=t)),p+1],nrow=n,byrow=T)
    y2=sub[seq(from=2,by=t,length.out=n),i]
    cv.fit_2=lm(y2~x)
    
    sub=rbind(data1,data2,data4,data5,data7,data9,data11,data14);n=n1+n2+n4+n5+n7+n9+n11+n14
    x=matrix(sub[sort(sequence(nvec=rep(n,3),from=c(1,2,3),by=t)),p+1],nrow=n,byrow=T)
    y3=sub[seq(from=3,by=t,length.out=n),i]
    cv.fit_3=lm(y3~x)
    #imputation###################################################################################################
    sub=data2;n=n2
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data2a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_1234,newx=z,s=cv.fit_1234$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data2b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_124,newx=z,s=cv.fit_124$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data2c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_134,newx=z,s=cv.fit_134$lambda.min)
    
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data2d[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_234,newx=z,s=cv.fit_234$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data2e[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
    
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data2f[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_24,newx=z,s=cv.fit_24$lambda.min)
    
    z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data2g[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
    ####################################################################################################
    sub=data3;n=n3
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,3),from=c(1,2,3),by=t)),p+1],nrow=n,byrow=T))
    data3a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_123,newx=z,s=cv.fit_123$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,3),from=c(1,2,3),by=t)),p+1],nrow=n,byrow=T))
    data3b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
    
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,3),from=c(1,2,3),by=t)),p+1],nrow=n,byrow=T))
    data3c[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_23,newx=z,s=cv.fit_23$lambda.min)
    ###################################################################################################
    sub=data4;n=n4
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,2),from=c(1,2),by=t)),p+1],nrow=n,byrow=T))
    data4a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
    ##################################################################################################################
    sub=data5;n=n5
    z=sub[seq(from=1,by=t,length.out=n),p+1]
    data5a[seq(from=1,by=t,length.out=n),i]=cbind(1,z)%*%cv.fit_1$coefficients
    ########################################################################################################
    sub=data6;n=n6
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,3),from=c(1,2,3),by=t)),p+1],nrow=n,byrow=T))
    data6a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_123,newx=z,s=cv.fit_123$lambda.min)
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data6a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_124,newx=z,s=cv.fit_124$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,3),from=c(1,2,3),by=t)),p+1],nrow=n,byrow=T))
    data6b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data6b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
    
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,3),from=c(1,2,3),by=t)),p+1],nrow=n,byrow=T))
    data6c[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_23,newx=z,s=cv.fit_23$lambda.min)
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data6c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_24,newx=z,s=cv.fit_24$lambda.min)
    ############################################################################
    sub=data7;n=n7
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,2),from=c(1,2),by=t)),p+1],nrow=n,byrow=T))
    data7a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data7a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_134,newx=z,s=cv.fit_134$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,2),from=c(1,2),by=t)),p+1],nrow=n,byrow=T))
    data7b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data7b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,2),from=c(1,2),by=t)),p+1],nrow=n,byrow=T))
    data7c[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
    z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data7c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
    ###############################################################################
    sub=data8;n=n8
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,2),from=c(1,2),by=t)),p+1],nrow=n,byrow=T))
    data8a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,3),from=c(1,2,3),by=t)),p+1],nrow=n,byrow=T))
    data8a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
    ##################################################################################
    sub=data9;n=n9
    z=sub[seq(from=1,by=t,length.out=n),p+1]
    data9a[seq(from=1,by=t,length.out=n),i]=cbind(1,z)%*%cv.fit_1$coefficients
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data9a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_234,newx=z,s=cv.fit_234$lambda.min)
    
    z=sub[seq(from=1,by=t,length.out=n),p+1]
    data9b[seq(from=1,by=t,length.out=n),i]=cbind(1,z)%*%cv.fit_1$coefficients
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data9b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_24,newx=z,s=cv.fit_24$lambda.min)
    
    z=sub[seq(from=1,by=t,length.out=n),p+1]
    data9c[seq(from=1,by=t,length.out=n),i]=cbind(1,z)%*%cv.fit_1$coefficients
    z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data9c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
    #########################################################################################
    sub=data10;n=n10
    z=sub[seq(from=1,by=t,length.out=n),p+1]
    data10a[seq(from=1,by=t,length.out=n),i]=cbind(1,z)%*%cv.fit_1$coefficients
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,3),from=c(1,2,3),by=t)),p+1],nrow=n,byrow=T))
    data10a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_23,newx=z,s=cv.fit_23$lambda.min)
    #############################################################################################
    sub=data11;n=n11
    z=sub[seq(from=1,by=t,length.out=n),p+1]
    data11a[seq(from=1,by=t,length.out=n),i]=cbind(1,z)%*%cv.fit_1$coefficients
    z=matrix(sub[sort(sequence(nvec=rep(n,2),from=c(1,2),by=t)),p+1],nrow=n,byrow=T)
    data11a[seq(from=2,by=t,length.out=n),i]=cbind(1,z)%*%cv.fit_2$coefficients
    ####################################################################################################
    sub=data12;n=n12
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,2),from=c(1,2),by=t)),p+1],nrow=n,byrow=T))
    data12a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,3),from=c(1,2,3),by=t)),p+1],nrow=n,byrow=T))
    data12a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data12a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
    
    sub=data13;n=n13
    z=sub[seq(from=1,by=t,length.out=n),p+1]
    data13a[seq(from=1,by=t,length.out=n),i]=cbind(1,z)%*%cv.fit_1$coefficients
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[sort(sequence(nvec=rep(n,3),from=c(1,2,3),by=t)),p+1],nrow=n,byrow=T))
    data13a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_23,newx=z,s=cv.fit_23$lambda.min)
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data13a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_24,newx=z,s=cv.fit_24$lambda.min)
    
    sub=data14;n=n14
    z=sub[seq(from=1,by=t,length.out=n),p+1]
    data14a[seq(from=1,by=t,length.out=n),i]=cbind(1,z)%*%cv.fit_1$coefficients
    z=matrix(sub[sort(sequence(nvec=rep(n,2),from=c(1,2),by=t)),p+1],nrow=n,byrow=T)
    data14a[seq(from=2,by=t,length.out=n),i]=cbind(1,z)%*%cv.fit_2$coefficients
    z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data14a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
    
    sub=data15;n=n15
    z=sub[seq(from=1,by=t,length.out=n),p+1]
    data15a[seq(from=1,by=t,length.out=n),i]=cbind(1,z)%*%cv.fit_1$coefficients
    z=matrix(sub[sort(sequence(nvec=rep(n,2),from=c(1,2),by=t)),p+1],nrow=n,byrow=T)
    data15a[seq(from=2,by=t,length.out=n),i]=cbind(1,z)%*%cv.fit_2$coefficients
    z=matrix(sub[sort(sequence(nvec=rep(n,3),from=c(1,2,3),by=t)),p+1],nrow=n,byrow=T)
    data15a[seq(from=3,by=t,length.out=n),i]=cbind(1,z)%*%cv.fit_3$coefficients
    #########################################################################################################
  }
  data1=list(data1a)
  data2=list(data2a,data2b,data2c,data2d,data2e,data2f,data2g)
  data3=list(data3a,data3b,data3c)
  data4=list(data4a)
  data5=list(data5a)
  data6=list(data6a,data6b,data6c)
  data7=list(data7a,data7b,data7c)
  data8=list(data8a)
  data9=list(data9a,data9b,data9c)
  data10=list(data10a)
  data11=list(data11a)
  data12=list(data12a)
  data13=list(data13a)
  data14=list(data14a)
  data15=list(data15a)
  data_lmi1=list(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12,data13,data14,data15)
  #####################################################################################################################################
  for (i in 1:15) {
    sub=data[which(data[,ncol(data)]==i),]
    assign(paste0('data',i),sub)
    assign(paste0('n',i),nrow(sub)/t)
  }
  for (i in c('a','b','c','d','e','f','g')) { assign(paste0('data2',i),data2) }
  for (i in c('a','b','c','d','e','f','g')) { assign(paste0('data3',i),data3) }
  for (i in c('a','b','c','d','e','f','g')) { assign(paste0('data4',i),data4) }
  for (i in c('a','b','c','d','e','f','g')) { assign(paste0('data5',i),data5) }
  
  for (i in c('a','b','c')) { assign(paste0('data6',i),data6) }
  for (i in c('a','b','c')) { assign(paste0('data7',i),data7) }
  for (i in c('a','b','c')) { assign(paste0('data8',i),data8) }
  for (i in c('a','b','c')) { assign(paste0('data9',i),data9) }
  for (i in c('a','b','c')) { assign(paste0('data10',i),data10) }
  for (i in c('a','b','c')) { assign(paste0('data11',i),data11) }
  
  data1a=data1
  data12a=data12
  data13a=data13
  data14a=data14
  data15a=data15
  #impute each dimension of mediators##############################################################################################################
  for (i in 1:p) {
    #learn correlations through regression
    sub=data1;n=n1
    x123=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    x124=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    x134=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    x234=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y1=sub[seq(from=1,by=t,length.out=n),i]
    y2=sub[seq(from=2,by=t,length.out=n),i]
    y3=sub[seq(from=3,by=t,length.out=n),i]
    y4=sub[seq(from=4,by=t,length.out=n),i]
    
    cv.fit_1234=cv.glmnet(x123,y4)
    cv.fit_1243=cv.glmnet(x124,y3)
    cv.fit_1342=cv.glmnet(x134,y2)
    cv.fit_2341=cv.glmnet(x234,y1)
    #################################################
    sub=rbind(data1,data2);n=n1+n2
    x12=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    x13=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    x23=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y1=sub[seq(from=1,by=t,length.out=n),i]
    y2=sub[seq(from=2,by=t,length.out=n),i]
    y3=sub[seq(from=3,by=t,length.out=n),i]
    cv.fit_123=cv.glmnet(x12,y3)
    cv.fit_132=cv.glmnet(x13,y2)
    cv.fit_231=cv.glmnet(x23,y1)
    
    sub=rbind(data1,data3);n=n1+n3
    x12=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    x14=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    x24=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y1=sub[seq(from=1,by=t,length.out=n),i]
    y2=sub[seq(from=2,by=t,length.out=n),i]
    y4=sub[seq(from=4,by=t,length.out=n),i]
    cv.fit_124=cv.glmnet(x12,y4)
    cv.fit_142=cv.glmnet(x14,y2)
    cv.fit_241=cv.glmnet(x24,y1)
    
    sub=rbind(data1,data4);n=n1+n4
    x13=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    x14=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    x34=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y1=sub[seq(from=1,by=t,length.out=n),i]
    y3=sub[seq(from=3,by=t,length.out=n),i]
    y4=sub[seq(from=4,by=t,length.out=n),i]
    cv.fit_134=cv.glmnet(x13,y4)
    cv.fit_143=cv.glmnet(x14,y3)
    cv.fit_341=cv.glmnet(x34,y1)
    
    sub=rbind(data1,data5);n=n1+n5
    x23=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    x24=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    x34=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y2=sub[seq(from=2,by=t,length.out=n),i]
    y3=sub[seq(from=3,by=t,length.out=n),i]
    y4=sub[seq(from=4,by=t,length.out=n),i]
    cv.fit_234=cv.glmnet(x23,y4)
    cv.fit_243=cv.glmnet(x24,y3)
    cv.fit_342=cv.glmnet(x34,y2)
    ##############################################################################################
    sub=rbind(data1,data2,data3,data6);n=n1+n2+n3+n6
    x1=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    x2=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y1=sub[seq(from=1,by=t,length.out=n),i]
    y2=sub[seq(from=2,by=t,length.out=n),i]
    cv.fit_12=cv.glmnet(x1,y2)
    cv.fit_21=cv.glmnet(x2,y1)
    
    sub=rbind(data1,data2,data4,data7);n=n1+n2+n4+n7
    x1=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    x3=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y1=sub[seq(from=1,by=t,length.out=n),i]
    y3=sub[seq(from=3,by=t,length.out=n),i]
    cv.fit_13=cv.glmnet(x1,y3)
    cv.fit_31=cv.glmnet(x3,y1)
    
    sub=rbind(data1,data3,data4,data8);n=n1+n3+n4+n8
    x1=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    x4=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y1=sub[seq(from=1,by=t,length.out=n),i]
    y4=sub[seq(from=4,by=t,length.out=n),i]
    cv.fit_14=cv.glmnet(x1,y4)
    cv.fit_41=cv.glmnet(x4,y1)
    
    sub=rbind(data1,data2,data5,data9);n=n1+n2+n5+n9
    x2=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    x3=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y2=sub[seq(from=2,by=t,length.out=n),i]
    y3=sub[seq(from=3,by=t,length.out=n),i]
    cv.fit_23=cv.glmnet(x2,y3)
    cv.fit_32=cv.glmnet(x3,y2)
    
    sub=rbind(data1,data3,data5,data10);n=n1+n3+n5+n10
    x2=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    x4=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y2=sub[seq(from=2,by=t,length.out=n),i]
    y4=sub[seq(from=4,by=t,length.out=n),i]
    cv.fit_24=cv.glmnet(x2,y4)
    cv.fit_42=cv.glmnet(x4,y2)
    
    sub=rbind(data1,data4,data5,data11);n=n1+n4+n5+n11
    x3=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    x4=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    y3=sub[seq(from=3,by=t,length.out=n),i]
    y4=sub[seq(from=4,by=t,length.out=n),i]
    cv.fit_34=cv.glmnet(x3,y4)
    cv.fit_43=cv.glmnet(x4,y3)
    #imputation###################################################################################################
    sub=data2;n=n2
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data2a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_1234,newx=z,s=cv.fit_1234$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data2b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_124,newx=z,s=cv.fit_124$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data2c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_134,newx=z,s=cv.fit_134$lambda.min)
    
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data2d[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_234,newx=z,s=cv.fit_234$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data2e[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
    
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data2f[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_24,newx=z,s=cv.fit_24$lambda.min)
    
    z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data2g[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
    ####################################################################################################
    sub=data3;n=n3
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data3a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_1243,newx=z,s=cv.fit_1243$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data3b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_123,newx=z,s=cv.fit_123$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data3c[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_143,newx=z,s=cv.fit_143$lambda.min)
    
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data3d[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_243,newx=z,s=cv.fit_243$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data3e[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
    
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data3f[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_23,newx=z,s=cv.fit_23$lambda.min)
    
    z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data3g[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_43,newx=z,s=cv.fit_43$lambda.min)
    ###################################################################################################
    sub=data4;n=n4
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data4a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_1342,newx=z,s=cv.fit_1342$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data4b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_132,newx=z,s=cv.fit_132$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data4c[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_142,newx=z,s=cv.fit_142$lambda.min)
    
    z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data4d[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_342,newx=z,s=cv.fit_342$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data4e[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
    
    z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data4f[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_32,newx=z,s=cv.fit_32$lambda.min)
    
    z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data4g[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_42,newx=z,s=cv.fit_42$lambda.min)
    ##################################################################################################################
    sub=data5;n=n5
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data5a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_2341,newx=z,s=cv.fit_2341$lambda.min)
    
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data5b[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_231,newx=z,s=cv.fit_231$lambda.min)
    
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data5c[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_241,newx=z,s=cv.fit_241$lambda.min)
    
    z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data5d[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_341,newx=z,s=cv.fit_341$lambda.min)
    
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data5e[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_21,newx=z,s=cv.fit_21$lambda.min)
    
    z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data5f[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_31,newx=z,s=cv.fit_31$lambda.min)
    
    z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data5g[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_41,newx=z,s=cv.fit_41$lambda.min)
    ########################################################################################################
    sub=data6;n=n6
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data6a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_123,newx=z,s=cv.fit_123$lambda.min)
    data6a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_124,newx=z,s=cv.fit_124$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data6b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
    data6b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
    
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data6c[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_23,newx=z,s=cv.fit_23$lambda.min)
    data6c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_24,newx=z,s=cv.fit_24$lambda.min)
    ############################################################################
    sub=data7;n=n7
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data7a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_132,newx=z,s=cv.fit_132$lambda.min)
    data7a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_134,newx=z,s=cv.fit_134$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data7b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
    data7b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
    
    z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data7c[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_32,newx=z,s=cv.fit_32$lambda.min)
    data7c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
    ###############################################################################
    sub=data8;n=n8
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data8a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_142,newx=z,s=cv.fit_142$lambda.min)
    data8a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_143,newx=z,s=cv.fit_143$lambda.min)
    
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data8b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
    data8b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
    
    z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data8c[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_42,newx=z,s=cv.fit_42$lambda.min)
    data8c[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_43,newx=z,s=cv.fit_43$lambda.min)
    ##################################################################################
    sub=data9;n=n9
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data9a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_231,newx=z,s=cv.fit_231$lambda.min)
    data9a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_234,newx=z,s=cv.fit_234$lambda.min)
    
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data9b[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_21,newx=z,s=cv.fit_21$lambda.min)
    data9b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_24,newx=z,s=cv.fit_24$lambda.min)
    
    z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data9c[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_31,newx=z,s=cv.fit_31$lambda.min)
    data9c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
    #########################################################################################
    sub=data10;n=n10
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data10a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_241,newx=z,s=cv.fit_241$lambda.min)
    data10a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_243,newx=z,s=cv.fit_243$lambda.min)
    
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data10b[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_21,newx=z,s=cv.fit_21$lambda.min)
    data10b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_23,newx=z,s=cv.fit_23$lambda.min)
    
    z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data10c[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_41,newx=z,s=cv.fit_41$lambda.min)
    data10c[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_43,newx=z,s=cv.fit_43$lambda.min)
    #############################################################################################
    sub=data11;n=n11
    z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data11a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_341,newx=z,s=cv.fit_341$lambda.min)
    data11a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_342,newx=z,s=cv.fit_342$lambda.min)
    
    z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data11b[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_31,newx=z,s=cv.fit_31$lambda.min)
    data11b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_32,newx=z,s=cv.fit_32$lambda.min)
    
    z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data11c[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_41,newx=z,s=cv.fit_41$lambda.min)
    data11c[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_42,newx=z,s=cv.fit_42$lambda.min)
    ####################################################################################################
    sub=data12;n=n12
    z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data12a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
    data12a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
    data12a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
    
    sub=data13;n=n13
    z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data13a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_21,newx=z,s=cv.fit_21$lambda.min)
    data13a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_23,newx=z,s=cv.fit_23$lambda.min)
    data13a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_24,newx=z,s=cv.fit_24$lambda.min)
    
    sub=data14;n=n14
    z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data14a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_31,newx=z,s=cv.fit_31$lambda.min)
    data14a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_32,newx=z,s=cv.fit_32$lambda.min)
    data14a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
    
    sub=data15;n=n15
    z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
    data15a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_41,newx=z,s=cv.fit_41$lambda.min)
    data15a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_42,newx=z,s=cv.fit_42$lambda.min)
    data15a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_43,newx=z,s=cv.fit_43$lambda.min)
    #########################################################################################################
  }
  data1=list(data1a)
  data2=list(data2a,data2b,data2c,data2d,data2e,data2f,data2g)
  data3=list(data3a,data3b,data3c,data3d,data3e,data3f,data3g)
  data4=list(data4a,data4b,data4c,data4d,data4e,data4f,data4g)
  data5=list(data5a,data5b,data5c,data5d,data5e,data5f,data5g)
  data6=list(data6a,data6b,data6c)
  data7=list(data7a,data7b,data7c)
  data8=list(data8a,data8b,data8c)
  data9=list(data9a,data9b,data9c)
  data10=list(data10a,data10b,data10c)
  data11=list(data11a,data11b,data11c)
  data12=list(data12a)
  data13=list(data13a)
  data14=list(data14a)
  data15=list(data15a)
  data_lmi2=list(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12,data13,data14,data15)
  return(list(data_lmi1,data_lmi2))
}