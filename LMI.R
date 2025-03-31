LMI=function(data,p,t,patt){
  
  if(patt=="A"){ #missingness structure (A)
    for (i in 1:4) {
      sub=data[which(data[,ncol(data)]==i),]
      assign(paste0('data',i),sub)
      assign(paste0('n',i),nrow(sub)/t)
    }
    data1a=data1
    data2a=data2
    data2b=data2
    data3a=data3
    data3b=data3
    data4a=data4
    #impute each dimension of mediators##############################################################################################################
    for (i in 1:p) {
      #learn correlations through regression
      sub=data1;n=n1
      x123=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x34=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y2=sub[seq(from=2,by=t,length.out=n),i]
      y4=sub[seq(from=4,by=t,length.out=n),i]
      cv.fit_1234=cv.glmnet(x123,y4)
      cv.fit_341=cv.glmnet(x34,y1)
      cv.fit_342=cv.glmnet(x34,y2)
      
      sub=rbind(data1,data2);n=n1+n2
      x3=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y2=sub[seq(from=2,by=t,length.out=n),i]
      cv.fit_31=cv.glmnet(x3,y1)
      cv.fit_32=cv.glmnet(x3,y2)
      
      sub=rbind(data1,data3);n=n1+n3
      x3=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y4=sub[seq(from=4,by=t,length.out=n),i]
      cv.fit_34=cv.glmnet(x3,y4)
      #imputation###################################################################################################
      sub=data2;n=n2
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_1234,newx=z,s=cv.fit_1234$lambda.min)
      
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
      #############################################################################################
      sub=data3;n=n3
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_341,newx=z,s=cv.fit_341$lambda.min)
      data3a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_342,newx=z,s=cv.fit_342$lambda.min)
      
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3b[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_31,newx=z,s=cv.fit_31$lambda.min)
      data3b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_32,newx=z,s=cv.fit_32$lambda.min)
      ####################################################################################################
      sub=data4;n=n4
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data4a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_31,newx=z,s=cv.fit_31$lambda.min)
      data4a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_32,newx=z,s=cv.fit_32$lambda.min)
      data4a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
      #########################################################################################################
    }
    #multiple imputed data in groups 1-4
    data1=list(data1a)  
    data2=list(data2a,data2b)
    data3=list(data3a,data3b)
    data4=list(data4a)
    data_lmi=list(data1,data2,data3,data4)
  }else if(patt=="B"){ #missingness structure (B)
    for (i in 1:6) {
      sub=data[which(data[,ncol(data)]==i),]
      assign(paste0('data',i),sub)
      assign(paste0('n',i),nrow(sub)/t)
    }
    data1a=data1
    data1b=data1
    data2a=data2
    data2b=data2
    data3a=data3
    data3b=data3
    data4a=data4
    data5a=data5
    data6a=data6
    ###############################################################################################################
    for (i in 1:p) {
      
      sub=data1;n=n1
      x1=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x23=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y2=sub[seq(from=2,by=t,length.out=n),i]
      y3=sub[seq(from=3,by=t,length.out=n),i]
      cv.fit_231=cv.glmnet(x23,y1)
      cv.fit_12=cv.glmnet(x1,y2)
      cv.fit_13=cv.glmnet(x1,y3)
      
      sub=data2;n=n2
      x23=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x4=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y2=sub[seq(from=2,by=t,length.out=n),i]
      y3=sub[seq(from=3,by=t,length.out=n),i]
      y4=sub[seq(from=4,by=t,length.out=n),i]
      cv.fit_234=cv.glmnet(x23,y4)
      cv.fit_42=cv.glmnet(x4,y2)
      cv.fit_43=cv.glmnet(x4,y3)
      
      sub=data3;n=n3
      x1=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x4=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y4=sub[seq(from=4,by=t,length.out=n),i]
      cv.fit_14=cv.glmnet(x1,y4)
      cv.fit_41=cv.glmnet(x4,y1)
      ####################################################################################################
      sub=data1;n=n1
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data1a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_234,newx=z,s=cv.fit_234$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data1b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
      ####################################################################################################
      sub=data2;n=n2
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_231,newx=z,s=cv.fit_231$lambda.min)
      
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2b[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_41,newx=z,s=cv.fit_41$lambda.min)
      #############################################################################################
      sub=data3;n=n3
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
      data3a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
      
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_42,newx=z,s=cv.fit_42$lambda.min)
      data3b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_43,newx=z,s=cv.fit_43$lambda.min)
      ####################################################################################################
      sub=data4;n=n4
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data4a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_231,newx=z,s=cv.fit_231$lambda.min)
      data4a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_234,newx=z,s=cv.fit_234$lambda.min)
      ####################################################################################################
      sub=data5;n=n5
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data5a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
      data5a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
      data5a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
      ####################################################################################################
      sub=data6;n=n6
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data6a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_41,newx=z,s=cv.fit_41$lambda.min)
      data6a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_42,newx=z,s=cv.fit_42$lambda.min)
      data6a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_43,newx=z,s=cv.fit_43$lambda.min)
      #########################################################################################################
    }
    data1=list(data1a,data1b)
    data2=list(data2a,data2b)
    data3=list(data3a,data3b)
    data4=list(data4a)
    data5=list(data5a)
    data6=list(data6a)
    data_lmi=list(data1,data2,data3,data4,data5,data6)
  }else if(patt=="T4"){ #missingness structure (T4)
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
    ###############################################################################################################
    for (i in 1:p) {
      
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
      ####################################################################################################
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
    data_lmi=list(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12,data13,data14,data15)
  }else if(patt=="T5"){ #missingness structure (T5)
    for (i in 1:31) {
      sub=data[which(data[,ncol(data)]==i),]
      assign(paste0('data',i),sub)
      assign(paste0('n',i),nrow(sub)/t)
    }
    
    for (i in c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o')) { assign(paste0('data2',i),data2) }
    for (i in c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o')) { assign(paste0('data3',i),data3) }
    for (i in c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o')) { assign(paste0('data4',i),data4) }
    for (i in c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o')) { assign(paste0('data5',i),data5) }
    for (i in c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o')) { assign(paste0('data6',i),data6) }
    
    for (i in c('a','b','c','d','e','f','g')) { assign(paste0('data7',i),data7) }
    for (i in c('a','b','c','d','e','f','g')) { assign(paste0('data8',i),data8) }
    for (i in c('a','b','c','d','e','f','g')) { assign(paste0('data9',i),data9) }
    for (i in c('a','b','c','d','e','f','g')) { assign(paste0('data10',i),data10) }
    for (i in c('a','b','c','d','e','f','g')) { assign(paste0('data11',i),data11) }
    for (i in c('a','b','c','d','e','f','g')) { assign(paste0('data12',i),data12) }
    for (i in c('a','b','c','d','e','f','g')) { assign(paste0('data13',i),data13) }
    for (i in c('a','b','c','d','e','f','g')) { assign(paste0('data14',i),data14) }
    for (i in c('a','b','c','d','e','f','g')) { assign(paste0('data15',i),data15) }
    for (i in c('a','b','c','d','e','f','g')) { assign(paste0('data16',i),data16) }
    
    for (i in c('a','b','c')) { assign(paste0('data17',i),data17) }
    for (i in c('a','b','c')) { assign(paste0('data18',i),data18) }
    for (i in c('a','b','c')) { assign(paste0('data19',i),data19) }
    for (i in c('a','b','c')) { assign(paste0('data20',i),data20) }
    for (i in c('a','b','c')) { assign(paste0('data21',i),data21) }
    for (i in c('a','b','c')) { assign(paste0('data22',i),data22) }
    for (i in c('a','b','c')) { assign(paste0('data23',i),data23) }
    for (i in c('a','b','c')) { assign(paste0('data24',i),data24) }
    for (i in c('a','b','c')) { assign(paste0('data25',i),data25) }
    for (i in c('a','b','c')) { assign(paste0('data26',i),data26) }
    
    data1a=data1
    data27a=data27
    data28a=data28
    data29a=data29
    data30a=data30
    data31a=data31
    ###############################################################################################################
    for (i in 1:p) {
      
      sub=data1;n=nrow(sub)/t
      x1234=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x1235=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x1245=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x1345=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x2345=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y2=sub[seq(from=2,by=t,length.out=n),i]
      y3=sub[seq(from=3,by=t,length.out=n),i]
      y4=sub[seq(from=4,by=t,length.out=n),i]
      y5=sub[seq(from=5,by=t,length.out=n),i]
      cv.fit_12345=cv.glmnet(x1234,y5)
      cv.fit_12354=cv.glmnet(x1235,y4)
      cv.fit_12453=cv.glmnet(x1245,y3)
      cv.fit_13452=cv.glmnet(x1345,y2)
      cv.fit_23451=cv.glmnet(x2345,y1)
      ###############################################################################################################################
      sub=rbind(data1,data2);n=nrow(sub)/t
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
      
      sub=rbind(data1,data3);n=nrow(sub)/t
      x123=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x125=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x135=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x235=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y2=sub[seq(from=2,by=t,length.out=n),i]
      y3=sub[seq(from=3,by=t,length.out=n),i]
      y5=sub[seq(from=5,by=t,length.out=n),i]
      cv.fit_1235=cv.glmnet(x123,y5)
      cv.fit_1253=cv.glmnet(x125,y3)
      cv.fit_1352=cv.glmnet(x135,y2)
      cv.fit_2351=cv.glmnet(x235,y1)
      
      sub=rbind(data1,data4);n=nrow(sub)/t
      x124=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x125=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x145=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x245=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y2=sub[seq(from=2,by=t,length.out=n),i]
      y4=sub[seq(from=4,by=t,length.out=n),i]
      y5=sub[seq(from=5,by=t,length.out=n),i]
      cv.fit_1245=cv.glmnet(x124,y5)
      cv.fit_1254=cv.glmnet(x125,y4)
      cv.fit_1452=cv.glmnet(x145,y2)
      cv.fit_2451=cv.glmnet(x245,y1)
      
      sub=rbind(data1,data5);n=nrow(sub)/t
      x134=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x135=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x145=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x345=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y3=sub[seq(from=3,by=t,length.out=n),i]
      y4=sub[seq(from=4,by=t,length.out=n),i]
      y5=sub[seq(from=5,by=t,length.out=n),i]
      cv.fit_1345=cv.glmnet(x134,y5)
      cv.fit_1354=cv.glmnet(x135,y4)
      cv.fit_1453=cv.glmnet(x145,y3)
      cv.fit_3451=cv.glmnet(x345,y1)
      
      sub=rbind(data1,data6);n=nrow(sub)/t
      x234=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x235=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x245=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x345=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y2=sub[seq(from=2,by=t,length.out=n),i]
      y3=sub[seq(from=3,by=t,length.out=n),i]
      y4=sub[seq(from=4,by=t,length.out=n),i]
      y5=sub[seq(from=5,by=t,length.out=n),i]
      cv.fit_2345=cv.glmnet(x234,y5)
      cv.fit_2354=cv.glmnet(x235,y4)
      cv.fit_2453=cv.glmnet(x245,y3)
      cv.fit_3452=cv.glmnet(x345,y2)
      #####################################################################################################################
      sub=rbind(data1,data2,data3,data7);n=nrow(sub)/t
      x12=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x13=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x23=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y2=sub[seq(from=2,by=t,length.out=n),i]
      y3=sub[seq(from=3,by=t,length.out=n),i]
      cv.fit_123=cv.glmnet(x12,y3)
      cv.fit_132=cv.glmnet(x13,y2)
      cv.fit_231=cv.glmnet(x23,y1)
      
      sub=rbind(data1,data2,data4,data8);n=nrow(sub)/t
      x12=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x14=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x24=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y2=sub[seq(from=2,by=t,length.out=n),i]
      y4=sub[seq(from=4,by=t,length.out=n),i]
      cv.fit_124=cv.glmnet(x12,y4)
      cv.fit_142=cv.glmnet(x14,y2)
      cv.fit_241=cv.glmnet(x24,y1)
      
      sub=rbind(data1,data3,data4,data9);n=nrow(sub)/t
      x12=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x15=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x25=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y2=sub[seq(from=2,by=t,length.out=n),i]
      y5=sub[seq(from=5,by=t,length.out=n),i]
      cv.fit_125=cv.glmnet(x12,y5)
      cv.fit_152=cv.glmnet(x15,y2)
      cv.fit_251=cv.glmnet(x25,y1)
      
      sub=rbind(data1,data2,data5,data10);n=nrow(sub)/t
      x13=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x14=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x34=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y3=sub[seq(from=3,by=t,length.out=n),i]
      y4=sub[seq(from=4,by=t,length.out=n),i]
      cv.fit_134=cv.glmnet(x13,y4)
      cv.fit_143=cv.glmnet(x14,y3)
      cv.fit_341=cv.glmnet(x34,y1)
      
      sub=rbind(data1,data3,data5,data11);n=nrow(sub)/t
      x13=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x15=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x35=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y3=sub[seq(from=3,by=t,length.out=n),i]
      y5=sub[seq(from=5,by=t,length.out=n),i]
      cv.fit_135=cv.glmnet(x13,y5)
      cv.fit_153=cv.glmnet(x15,y3)
      cv.fit_351=cv.glmnet(x35,y1)
      
      sub=rbind(data1,data4,data5,data12);n=nrow(sub)/t
      x14=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x15=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x45=cbind(sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y4=sub[seq(from=4,by=t,length.out=n),i]
      y5=sub[seq(from=5,by=t,length.out=n),i]
      cv.fit_145=cv.glmnet(x14,y5)
      cv.fit_154=cv.glmnet(x15,y4)
      cv.fit_451=cv.glmnet(x45,y1)
      
      sub=rbind(data1,data2,data6,data13);n=nrow(sub)/t
      x23=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x24=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x34=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y2=sub[seq(from=2,by=t,length.out=n),i]
      y3=sub[seq(from=3,by=t,length.out=n),i]
      y4=sub[seq(from=4,by=t,length.out=n),i]
      cv.fit_234=cv.glmnet(x23,y4)
      cv.fit_243=cv.glmnet(x24,y3)
      cv.fit_342=cv.glmnet(x34,y2)
      
      sub=rbind(data1,data3,data6,data14);n=nrow(sub)/t
      x23=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x25=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x35=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y2=sub[seq(from=2,by=t,length.out=n),i]
      y3=sub[seq(from=3,by=t,length.out=n),i]
      y5=sub[seq(from=5,by=t,length.out=n),i]
      cv.fit_235=cv.glmnet(x23,y5)
      cv.fit_253=cv.glmnet(x25,y3)
      cv.fit_352=cv.glmnet(x35,y2)
      
      sub=rbind(data1,data4,data6,data15);n=nrow(sub)/t
      x24=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x25=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x45=cbind(sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y2=sub[seq(from=2,by=t,length.out=n),i]
      y4=sub[seq(from=4,by=t,length.out=n),i]
      y5=sub[seq(from=5,by=t,length.out=n),i]
      cv.fit_245=cv.glmnet(x24,y5)
      cv.fit_254=cv.glmnet(x25,y4)
      cv.fit_452=cv.glmnet(x45,y2)
      
      sub=rbind(data1,data5,data6,data16);n=nrow(sub)/t
      x34=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x35=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x45=cbind(sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y3=sub[seq(from=3,by=t,length.out=n),i]
      y4=sub[seq(from=4,by=t,length.out=n),i]
      y5=sub[seq(from=5,by=t,length.out=n),i]
      cv.fit_345=cv.glmnet(x34,y5)
      cv.fit_354=cv.glmnet(x35,y4)
      cv.fit_453=cv.glmnet(x45,y3)
      ########################################################################################################################
      sub=rbind(data1,data2,data3,data4,data7,data8,data9,data17);n=nrow(sub)/t
      x1=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x2=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y2=sub[seq(from=2,by=t,length.out=n),i]
      cv.fit_12=cv.glmnet(x1,y2)
      cv.fit_21=cv.glmnet(x2,y1)
      
      sub=rbind(data1,data2,data3,data5,data7,data10,data11,data18);n=nrow(sub)/t
      x1=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x3=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y3=sub[seq(from=3,by=t,length.out=n),i]
      cv.fit_13=cv.glmnet(x1,y3)
      cv.fit_31=cv.glmnet(x3,y1)
      
      sub=rbind(data1,data2,data4,data5,data8,data10,data12,data19);n=nrow(sub)/t
      x1=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x4=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y4=sub[seq(from=4,by=t,length.out=n),i]
      cv.fit_14=cv.glmnet(x1,y4)
      cv.fit_41=cv.glmnet(x4,y1)
      
      sub=rbind(data1,data3,data4,data5,data9,data11,data12,data20);n=nrow(sub)/t
      x1=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x5=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y1=sub[seq(from=1,by=t,length.out=n),i]
      y5=sub[seq(from=5,by=t,length.out=n),i]
      cv.fit_15=cv.glmnet(x1,y5)
      cv.fit_51=cv.glmnet(x5,y1)
      
      sub=rbind(data1,data2,data3,data6,data7,data13,data14,data21);n=nrow(sub)/t
      x2=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x3=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y2=sub[seq(from=2,by=t,length.out=n),i]
      y3=sub[seq(from=3,by=t,length.out=n),i]
      cv.fit_23=cv.glmnet(x2,y3)
      cv.fit_32=cv.glmnet(x3,y2)
      
      sub=rbind(data1,data2,data4,data6,data8,data13,data15,data22);n=nrow(sub)/t
      x2=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x4=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y2=sub[seq(from=2,by=t,length.out=n),i]
      y4=sub[seq(from=4,by=t,length.out=n),i]
      cv.fit_24=cv.glmnet(x2,y4)
      cv.fit_42=cv.glmnet(x4,y2)
      
      sub=rbind(data1,data3,data4,data6,data9,data14,data15,data23);n=nrow(sub)/t
      x2=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x5=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y2=sub[seq(from=2,by=t,length.out=n),i]
      y5=sub[seq(from=5,by=t,length.out=n),i]
      cv.fit_25=cv.glmnet(x2,y5)
      cv.fit_52=cv.glmnet(x5,y2)
      
      sub=rbind(data1,data2,data5,data6,data10,data13,data16,data24);n=nrow(sub)/t
      x3=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x4=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y3=sub[seq(from=3,by=t,length.out=n),i]
      y4=sub[seq(from=4,by=t,length.out=n),i]
      cv.fit_34=cv.glmnet(x3,y4)
      cv.fit_43=cv.glmnet(x4,y3)
      
      sub=rbind(data1,data3,data5,data6,data11,data14,data16,data25);n=nrow(sub)/t
      x3=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x5=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y3=sub[seq(from=3,by=t,length.out=n),i]
      y5=sub[seq(from=5,by=t,length.out=n),i]
      cv.fit_35=cv.glmnet(x3,y5)
      cv.fit_53=cv.glmnet(x5,y3)
      
      sub=rbind(data1,data4,data5,data6,data12,data15,data16,data26);n=nrow(sub)/t
      x4=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      x5=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      y4=sub[seq(from=4,by=t,length.out=n),i]
      y5=sub[seq(from=5,by=t,length.out=n),i]
      cv.fit_45=cv.glmnet(x4,y5)
      cv.fit_54=cv.glmnet(x5,y4)
      #################################################################################################################
      sub=data2;n=nrow(sub)/t
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2a[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_12345,newx=z,s=cv.fit_12345$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2b[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_1235,newx=z,s=cv.fit_1235$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2c[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_1245,newx=z,s=cv.fit_1245$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2d[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_1345,newx=z,s=cv.fit_1345$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2e[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_2345,newx=z,s=cv.fit_2345$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2f[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_125,newx=z,s=cv.fit_125$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2g[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_135,newx=z,s=cv.fit_135$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2h[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_145,newx=z,s=cv.fit_145$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2i[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_235,newx=z,s=cv.fit_235$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2j[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_245,newx=z,s=cv.fit_245$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2k[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_345,newx=z,s=cv.fit_345$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2l[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_15,newx=z,s=cv.fit_15$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2m[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_25,newx=z,s=cv.fit_25$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2n[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_35,newx=z,s=cv.fit_35$lambda.min)
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data2o[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_45,newx=z,s=cv.fit_45$lambda.min)
      ##########################################################################################################
      sub=data3;n=nrow(sub)/t
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_12354,newx=z,s=cv.fit_12354$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_1234,newx=z,s=cv.fit_1234$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_1254,newx=z,s=cv.fit_1254$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3d[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_1354,newx=z,s=cv.fit_1354$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3e[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_2354,newx=z,s=cv.fit_2354$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3f[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_124,newx=z,s=cv.fit_124$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3g[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_134,newx=z,s=cv.fit_134$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3h[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_154,newx=z,s=cv.fit_154$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3i[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_234,newx=z,s=cv.fit_234$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3j[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_254,newx=z,s=cv.fit_254$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3k[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_354,newx=z,s=cv.fit_354$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3l[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3m[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_24,newx=z,s=cv.fit_24$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3n[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
      z=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data3o[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_54,newx=z,s=cv.fit_54$lambda.min)
      ##########################################################################################################
      sub=data4;n=nrow(sub)/t
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data4a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_12453,newx=z,s=cv.fit_12453$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data4b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_1243,newx=z,s=cv.fit_1243$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data4c[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_1253,newx=z,s=cv.fit_1253$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data4d[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_1453,newx=z,s=cv.fit_1453$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data4e[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_2453,newx=z,s=cv.fit_2453$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data4f[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_123,newx=z,s=cv.fit_123$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data4g[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_143,newx=z,s=cv.fit_143$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data4h[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_153,newx=z,s=cv.fit_153$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data4i[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_243,newx=z,s=cv.fit_243$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data4j[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_253,newx=z,s=cv.fit_253$lambda.min)
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data4k[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_453,newx=z,s=cv.fit_453$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data4l[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data4m[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_23,newx=z,s=cv.fit_23$lambda.min)
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data4n[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_43,newx=z,s=cv.fit_43$lambda.min)
      z=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data4o[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_53,newx=z,s=cv.fit_53$lambda.min)
      ##########################################################################################################
      sub=data5;n=nrow(sub)/t
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data5a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_13452,newx=z,s=cv.fit_13452$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data5b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_1342,newx=z,s=cv.fit_1342$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data5c[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_1352,newx=z,s=cv.fit_1352$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data5d[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_1452,newx=z,s=cv.fit_1452$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data5e[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_3452,newx=z,s=cv.fit_3452$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data5f[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_132,newx=z,s=cv.fit_132$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data5g[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_142,newx=z,s=cv.fit_142$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data5h[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_152,newx=z,s=cv.fit_152$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data5i[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_342,newx=z,s=cv.fit_342$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data5j[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_352,newx=z,s=cv.fit_352$lambda.min)
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data5k[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_452,newx=z,s=cv.fit_452$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data5l[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data5m[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_32,newx=z,s=cv.fit_32$lambda.min)
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data5n[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_42,newx=z,s=cv.fit_42$lambda.min)
      z=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data5o[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_52,newx=z,s=cv.fit_52$lambda.min)
      ##########################################################################################################
      sub=data6;n=nrow(sub)/t
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data6a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_23451,newx=z,s=cv.fit_23451$lambda.min)
      
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data6b[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_2341,newx=z,s=cv.fit_2341$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data6c[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_2351,newx=z,s=cv.fit_2351$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data6d[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_2451,newx=z,s=cv.fit_2451$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data6e[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_3451,newx=z,s=cv.fit_3451$lambda.min)
      
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data6f[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_231,newx=z,s=cv.fit_231$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data6g[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_241,newx=z,s=cv.fit_241$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data6h[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_251,newx=z,s=cv.fit_251$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data6i[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_341,newx=z,s=cv.fit_341$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data6j[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_351,newx=z,s=cv.fit_351$lambda.min)
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data6k[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_451,newx=z,s=cv.fit_451$lambda.min)
      
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data6l[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_21,newx=z,s=cv.fit_21$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data6m[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_31,newx=z,s=cv.fit_31$lambda.min)
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data6n[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_41,newx=z,s=cv.fit_41$lambda.min)
      z=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data6o[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_51,newx=z,s=cv.fit_51$lambda.min)
      #########################################################################################################################################
      sub=data7;n=nrow(sub)/t
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data7a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_1234,newx=z,s=cv.fit_1234$lambda.min)
      data7a[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_1235,newx=z,s=cv.fit_1235$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data7b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_124,newx=z,s=cv.fit_124$lambda.min)
      data7b[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_125,newx=z,s=cv.fit_125$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data7c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_134,newx=z,s=cv.fit_134$lambda.min)
      data7c[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_135,newx=z,s=cv.fit_135$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data7d[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_234,newx=z,s=cv.fit_234$lambda.min)
      data7d[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_235,newx=z,s=cv.fit_235$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data7e[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
      data7e[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_15,newx=z,s=cv.fit_15$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data7f[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_24,newx=z,s=cv.fit_24$lambda.min)
      data7f[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_25,newx=z,s=cv.fit_25$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data7g[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
      data7g[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_35,newx=z,s=cv.fit_35$lambda.min)
      #########################################################################################################################################
      sub=data8;n=nrow(sub)/t
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data8a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_1243,newx=z,s=cv.fit_1243$lambda.min)
      data8a[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_1245,newx=z,s=cv.fit_1245$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data8b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_123,newx=z,s=cv.fit_123$lambda.min)
      data8b[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_125,newx=z,s=cv.fit_125$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data8c[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_143,newx=z,s=cv.fit_143$lambda.min)
      data8c[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_145,newx=z,s=cv.fit_145$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data8d[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_243,newx=z,s=cv.fit_243$lambda.min)
      data8d[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_245,newx=z,s=cv.fit_245$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data8e[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
      data8e[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_15,newx=z,s=cv.fit_15$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data8f[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_23,newx=z,s=cv.fit_23$lambda.min)
      data8f[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_25,newx=z,s=cv.fit_25$lambda.min)
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data8g[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_43,newx=z,s=cv.fit_43$lambda.min)
      data8g[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_45,newx=z,s=cv.fit_45$lambda.min)
      #########################################################################################################################################
      sub=data9;n=nrow(sub)/t
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data9a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_1253,newx=z,s=cv.fit_1253$lambda.min)
      data9a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_1254,newx=z,s=cv.fit_1254$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data9b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_123,newx=z,s=cv.fit_123$lambda.min)
      data9b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_124,newx=z,s=cv.fit_124$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data9c[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_153,newx=z,s=cv.fit_153$lambda.min)
      data9c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_154,newx=z,s=cv.fit_154$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data9d[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_253,newx=z,s=cv.fit_253$lambda.min)
      data9d[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_254,newx=z,s=cv.fit_254$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data9e[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
      data9e[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data9f[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_23,newx=z,s=cv.fit_23$lambda.min)
      data9f[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_24,newx=z,s=cv.fit_24$lambda.min)
      z=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data9g[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_53,newx=z,s=cv.fit_53$lambda.min)
      data9g[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_54,newx=z,s=cv.fit_54$lambda.min)
      #########################################################################################################################################
      sub=data10;n=nrow(sub)/t
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data10a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_1342,newx=z,s=cv.fit_1342$lambda.min)
      data10a[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_1345,newx=z,s=cv.fit_1345$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data10b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_132,newx=z,s=cv.fit_132$lambda.min)
      data10b[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_135,newx=z,s=cv.fit_135$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data10c[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_142,newx=z,s=cv.fit_142$lambda.min)
      data10c[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_145,newx=z,s=cv.fit_145$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data10d[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_342,newx=z,s=cv.fit_342$lambda.min)
      data10d[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_345,newx=z,s=cv.fit_345$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data10e[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
      data10e[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_15,newx=z,s=cv.fit_15$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data10f[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_32,newx=z,s=cv.fit_32$lambda.min)
      data10f[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_35,newx=z,s=cv.fit_35$lambda.min)
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data10g[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_42,newx=z,s=cv.fit_42$lambda.min)
      data10g[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_45,newx=z,s=cv.fit_45$lambda.min)
      #########################################################################################################################################
      sub=data11;n=nrow(sub)/t
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data11a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_1352,newx=z,s=cv.fit_1352$lambda.min)
      data11a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_1354,newx=z,s=cv.fit_1354$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data11b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_132,newx=z,s=cv.fit_132$lambda.min)
      data11b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_134,newx=z,s=cv.fit_134$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data11c[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_152,newx=z,s=cv.fit_152$lambda.min)
      data11c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_154,newx=z,s=cv.fit_154$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data11d[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_352,newx=z,s=cv.fit_352$lambda.min)
      data11d[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_354,newx=z,s=cv.fit_354$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data11e[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
      data11e[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data11f[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_32,newx=z,s=cv.fit_32$lambda.min)
      data11f[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
      z=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data11g[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_52,newx=z,s=cv.fit_52$lambda.min)
      data11g[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_54,newx=z,s=cv.fit_54$lambda.min)
      #########################################################################################################################################
      sub=data12;n=nrow(sub)/t
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data12a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_1452,newx=z,s=cv.fit_1452$lambda.min)
      data12a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_1453,newx=z,s=cv.fit_1453$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data12b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_142,newx=z,s=cv.fit_142$lambda.min)
      data12b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_143,newx=z,s=cv.fit_143$lambda.min)
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data12c[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_152,newx=z,s=cv.fit_152$lambda.min)
      data12c[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_153,newx=z,s=cv.fit_153$lambda.min)
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data12d[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_452,newx=z,s=cv.fit_452$lambda.min)
      data12d[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_453,newx=z,s=cv.fit_453$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data12e[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
      data12e[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data12f[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_42,newx=z,s=cv.fit_42$lambda.min)
      data12f[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_43,newx=z,s=cv.fit_43$lambda.min)
      z=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data12g[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_52,newx=z,s=cv.fit_52$lambda.min)
      data12g[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_53,newx=z,s=cv.fit_53$lambda.min)
      #########################################################################################################################################
      sub=data13;n=nrow(sub)/t
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data13a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_2341,newx=z,s=cv.fit_2341$lambda.min)
      data13a[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_2345,newx=z,s=cv.fit_2345$lambda.min)
      
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data13b[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_231,newx=z,s=cv.fit_231$lambda.min)
      data13b[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_235,newx=z,s=cv.fit_235$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data13c[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_241,newx=z,s=cv.fit_241$lambda.min)
      data13c[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_245,newx=z,s=cv.fit_245$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data13d[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_341,newx=z,s=cv.fit_341$lambda.min)
      data13d[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_345,newx=z,s=cv.fit_345$lambda.min)
      
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data13e[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_21,newx=z,s=cv.fit_21$lambda.min)
      data13e[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_25,newx=z,s=cv.fit_25$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data13f[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_31,newx=z,s=cv.fit_31$lambda.min)
      data13f[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_35,newx=z,s=cv.fit_35$lambda.min)
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data13g[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_41,newx=z,s=cv.fit_41$lambda.min)
      data13g[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_45,newx=z,s=cv.fit_45$lambda.min)
      #########################################################################################################################################
      sub=data14;n=nrow(sub)/t
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data14a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_2351,newx=z,s=cv.fit_2351$lambda.min)
      data14a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_2354,newx=z,s=cv.fit_2354$lambda.min)
      
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data14b[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_231,newx=z,s=cv.fit_231$lambda.min)
      data14b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_234,newx=z,s=cv.fit_234$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data14c[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_251,newx=z,s=cv.fit_251$lambda.min)
      data14c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_254,newx=z,s=cv.fit_254$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data14d[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_351,newx=z,s=cv.fit_351$lambda.min)
      data14d[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_354,newx=z,s=cv.fit_354$lambda.min)
      
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data14e[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_21,newx=z,s=cv.fit_21$lambda.min)
      data14e[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_24,newx=z,s=cv.fit_24$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data14f[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_31,newx=z,s=cv.fit_31$lambda.min)
      data14f[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
      z=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data14g[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_51,newx=z,s=cv.fit_51$lambda.min)
      data14g[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_54,newx=z,s=cv.fit_54$lambda.min)
      #########################################################################################################################################
      sub=data15;n=nrow(sub)/t
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data15a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_2451,newx=z,s=cv.fit_2451$lambda.min)
      data15a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_2453,newx=z,s=cv.fit_2453$lambda.min)
      
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data15b[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_241,newx=z,s=cv.fit_241$lambda.min)
      data15b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_243,newx=z,s=cv.fit_243$lambda.min)
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data15c[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_251,newx=z,s=cv.fit_251$lambda.min)
      data15c[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_253,newx=z,s=cv.fit_253$lambda.min)
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data15d[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_451,newx=z,s=cv.fit_451$lambda.min)
      data15d[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_453,newx=z,s=cv.fit_453$lambda.min)
      
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data15e[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_21,newx=z,s=cv.fit_21$lambda.min)
      data15e[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_23,newx=z,s=cv.fit_23$lambda.min)
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data15f[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_41,newx=z,s=cv.fit_41$lambda.min)
      data15f[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_43,newx=z,s=cv.fit_43$lambda.min)
      z=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data15g[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_51,newx=z,s=cv.fit_51$lambda.min)
      data15g[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_53,newx=z,s=cv.fit_53$lambda.min)
      #########################################################################################################################################
      sub=data16;n=nrow(sub)/t
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data16a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_3451,newx=z,s=cv.fit_3451$lambda.min)
      data16a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_3452,newx=z,s=cv.fit_3452$lambda.min)
      
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data16b[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_341,newx=z,s=cv.fit_341$lambda.min)
      data16b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_342,newx=z,s=cv.fit_342$lambda.min)
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data16c[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_351,newx=z,s=cv.fit_351$lambda.min)
      data16c[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_352,newx=z,s=cv.fit_352$lambda.min)
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data16d[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_451,newx=z,s=cv.fit_451$lambda.min)
      data16d[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_452,newx=z,s=cv.fit_452$lambda.min)
      
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data16e[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_31,newx=z,s=cv.fit_31$lambda.min)
      data16e[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_32,newx=z,s=cv.fit_32$lambda.min)
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data16f[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_41,newx=z,s=cv.fit_41$lambda.min)
      data16f[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_42,newx=z,s=cv.fit_42$lambda.min)
      z=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data16g[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_51,newx=z,s=cv.fit_51$lambda.min)
      data16g[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_52,newx=z,s=cv.fit_52$lambda.min)
      ########################################################################################################
      sub=data17;n=nrow(sub)/t
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data17a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_123,newx=z,s=cv.fit_123$lambda.min)
      data17a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_124,newx=z,s=cv.fit_124$lambda.min)
      data17a[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_125,newx=z,s=cv.fit_125$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data17b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
      data17b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
      data17b[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_15,newx=z,s=cv.fit_15$lambda.min)
      
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data17c[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_23,newx=z,s=cv.fit_23$lambda.min)
      data17c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_24,newx=z,s=cv.fit_24$lambda.min)
      data17c[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_25,newx=z,s=cv.fit_25$lambda.min)
      ########################################################################################################
      sub=data18;n=nrow(sub)/t
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data18a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_132,newx=z,s=cv.fit_132$lambda.min)
      data18a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_134,newx=z,s=cv.fit_134$lambda.min)
      data18a[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_135,newx=z,s=cv.fit_135$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data18b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
      data18b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
      data18b[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_15,newx=z,s=cv.fit_15$lambda.min)
      
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data18c[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_32,newx=z,s=cv.fit_32$lambda.min)
      data18c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
      data18c[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_35,newx=z,s=cv.fit_35$lambda.min)
      ########################################################################################################
      sub=data19;n=nrow(sub)/t
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data19a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_142,newx=z,s=cv.fit_142$lambda.min)
      data19a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_143,newx=z,s=cv.fit_143$lambda.min)
      data19a[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_145,newx=z,s=cv.fit_145$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data19b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
      data19b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
      data19b[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_15,newx=z,s=cv.fit_15$lambda.min)
      
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data19c[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_42,newx=z,s=cv.fit_42$lambda.min)
      data19c[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_43,newx=z,s=cv.fit_43$lambda.min)
      data19c[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_45,newx=z,s=cv.fit_45$lambda.min)
      ########################################################################################################
      sub=data20;n=nrow(sub)/t
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data20a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_152,newx=z,s=cv.fit_152$lambda.min)
      data20a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_153,newx=z,s=cv.fit_153$lambda.min)
      data20a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_154,newx=z,s=cv.fit_154$lambda.min)
      
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data20b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
      data20b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
      data20b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
      
      z=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data20c[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_52,newx=z,s=cv.fit_52$lambda.min)
      data20c[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_53,newx=z,s=cv.fit_53$lambda.min)
      data20c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_54,newx=z,s=cv.fit_54$lambda.min)
      ########################################################################################################
      sub=data21;n=nrow(sub)/t
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data21a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_231,newx=z,s=cv.fit_231$lambda.min)
      data21a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_234,newx=z,s=cv.fit_234$lambda.min)
      data21a[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_235,newx=z,s=cv.fit_235$lambda.min)
      
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data21b[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_21,newx=z,s=cv.fit_21$lambda.min)
      data21b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_24,newx=z,s=cv.fit_24$lambda.min)
      data21b[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_25,newx=z,s=cv.fit_25$lambda.min)
      
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data21c[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_31,newx=z,s=cv.fit_31$lambda.min)
      data21c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
      data21c[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_35,newx=z,s=cv.fit_35$lambda.min)
      ########################################################################################################
      sub=data22;n=nrow(sub)/t
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data22a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_241,newx=z,s=cv.fit_241$lambda.min)
      data22a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_243,newx=z,s=cv.fit_243$lambda.min)
      data22a[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_245,newx=z,s=cv.fit_245$lambda.min)
      
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data22b[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_21,newx=z,s=cv.fit_21$lambda.min)
      data22b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_23,newx=z,s=cv.fit_23$lambda.min)
      data22b[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_25,newx=z,s=cv.fit_25$lambda.min)
      
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data22c[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_41,newx=z,s=cv.fit_41$lambda.min)
      data22c[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_43,newx=z,s=cv.fit_43$lambda.min)
      data22c[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_45,newx=z,s=cv.fit_45$lambda.min)
      ########################################################################################################
      sub=data23;n=nrow(sub)/t
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data23a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_251,newx=z,s=cv.fit_251$lambda.min)
      data23a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_253,newx=z,s=cv.fit_253$lambda.min)
      data23a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_254,newx=z,s=cv.fit_254$lambda.min)
      
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data23b[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_21,newx=z,s=cv.fit_21$lambda.min)
      data23b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_23,newx=z,s=cv.fit_23$lambda.min)
      data23b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_24,newx=z,s=cv.fit_24$lambda.min)
      
      z=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data23c[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_51,newx=z,s=cv.fit_51$lambda.min)
      data23c[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_53,newx=z,s=cv.fit_53$lambda.min)
      data23c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_54,newx=z,s=cv.fit_54$lambda.min)
      ########################################################################################################
      sub=data24;n=nrow(sub)/t
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data24a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_341,newx=z,s=cv.fit_341$lambda.min)
      data24a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_342,newx=z,s=cv.fit_342$lambda.min)
      data24a[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_345,newx=z,s=cv.fit_345$lambda.min)
      
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data24b[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_31,newx=z,s=cv.fit_31$lambda.min)
      data24b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_32,newx=z,s=cv.fit_32$lambda.min)
      data24b[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_35,newx=z,s=cv.fit_35$lambda.min)
      
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data24c[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_41,newx=z,s=cv.fit_41$lambda.min)
      data24c[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_42,newx=z,s=cv.fit_42$lambda.min)
      data24c[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_45,newx=z,s=cv.fit_45$lambda.min)
      ########################################################################################################
      sub=data25;n=nrow(sub)/t
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data25a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_351,newx=z,s=cv.fit_351$lambda.min)
      data25a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_352,newx=z,s=cv.fit_352$lambda.min)
      data25a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_354,newx=z,s=cv.fit_354$lambda.min)
      
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data25b[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_31,newx=z,s=cv.fit_31$lambda.min)
      data25b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_32,newx=z,s=cv.fit_32$lambda.min)
      data25b[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
      
      z=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data25c[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_51,newx=z,s=cv.fit_51$lambda.min)
      data25c[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_52,newx=z,s=cv.fit_52$lambda.min)
      data25c[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_54,newx=z,s=cv.fit_54$lambda.min)
      ########################################################################################################
      sub=data26;n=nrow(sub)/t
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data26a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_451,newx=z,s=cv.fit_451$lambda.min)
      data26a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_452,newx=z,s=cv.fit_452$lambda.min)
      data26a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_453,newx=z,s=cv.fit_453$lambda.min)
      
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data26b[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_41,newx=z,s=cv.fit_41$lambda.min)
      data26b[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_42,newx=z,s=cv.fit_42$lambda.min)
      data26b[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_43,newx=z,s=cv.fit_43$lambda.min)
      
      z=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data26c[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_51,newx=z,s=cv.fit_51$lambda.min)
      data26c[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_52,newx=z,s=cv.fit_52$lambda.min)
      data26c[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_53,newx=z,s=cv.fit_53$lambda.min)
      #######################################################################################################################
      sub=data27;n=nrow(sub)/t
      z=cbind(sub[seq(from=1,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data27a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_12,newx=z,s=cv.fit_12$lambda.min)
      data27a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_13,newx=z,s=cv.fit_13$lambda.min)
      data27a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_14,newx=z,s=cv.fit_14$lambda.min)
      data27a[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_15,newx=z,s=cv.fit_15$lambda.min)
      
      sub=data28;n=nrow(sub)/t
      z=cbind(sub[seq(from=2,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data28a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_21,newx=z,s=cv.fit_21$lambda.min)
      data28a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_23,newx=z,s=cv.fit_23$lambda.min)
      data28a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_24,newx=z,s=cv.fit_24$lambda.min)
      data28a[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_25,newx=z,s=cv.fit_25$lambda.min)
      
      sub=data29;n=nrow(sub)/t
      z=cbind(sub[seq(from=3,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data29a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_31,newx=z,s=cv.fit_31$lambda.min)
      data29a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_32,newx=z,s=cv.fit_32$lambda.min)
      data29a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_34,newx=z,s=cv.fit_34$lambda.min)
      data29a[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_35,newx=z,s=cv.fit_35$lambda.min)
      
      sub=data30;n=nrow(sub)/t
      z=cbind(sub[seq(from=4,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data30a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_41,newx=z,s=cv.fit_41$lambda.min)
      data30a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_42,newx=z,s=cv.fit_42$lambda.min)
      data30a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_43,newx=z,s=cv.fit_43$lambda.min)
      data30a[seq(from=5,by=t,length.out=n),i]=predict(cv.fit_45,newx=z,s=cv.fit_45$lambda.min)
      
      sub=data31;n=nrow(sub)/t
      z=cbind(sub[seq(from=5,by=t,length.out=n),1:p],matrix(sub[,p+1],nrow=n,byrow=T))
      data31a[seq(from=1,by=t,length.out=n),i]=predict(cv.fit_51,newx=z,s=cv.fit_51$lambda.min)
      data31a[seq(from=2,by=t,length.out=n),i]=predict(cv.fit_52,newx=z,s=cv.fit_52$lambda.min)
      data31a[seq(from=3,by=t,length.out=n),i]=predict(cv.fit_53,newx=z,s=cv.fit_53$lambda.min)
      data31a[seq(from=4,by=t,length.out=n),i]=predict(cv.fit_54,newx=z,s=cv.fit_54$lambda.min)
      #########################################################################################################
    }
    data1=list(data1a)
    data2=list(data2a,data2b,data2c,data2d,data2e,data2f,data2g,data2h,data2i,data2j,data2k,data2l,data2m,data2n,data2o)
    data3=list(data3a,data3b,data3c,data3d,data3e,data3f,data3g,data3h,data3i,data3j,data3k,data3l,data3m,data3n,data3o)
    data4=list(data4a,data4b,data4c,data4d,data4e,data4f,data4g,data4h,data4i,data4j,data4k,data4l,data4m,data4n,data4o)
    data5=list(data5a,data5b,data5c,data5d,data5e,data5f,data5g,data5h,data5i,data5j,data5k,data5l,data5m,data5n,data5o)
    data6=list(data6a,data6b,data6c,data6d,data6e,data6f,data6g,data6h,data6i,data6j,data6k,data6l,data6m,data6n,data6o)
    
    data7=list(data7a,data7b,data7c,data7d,data7e,data7f,data7g)
    data8=list(data8a,data8b,data8c,data8d,data8e,data8f,data8g)
    data9=list(data9a,data9b,data9c,data9d,data9e,data9f,data9g)
    data10=list(data10a,data10b,data10c,data10d,data10e,data10f,data10g)
    data11=list(data11a,data11b,data11c,data11d,data11e,data11f,data11g)
    data12=list(data12a,data12b,data12c,data12d,data12e,data12f,data12g)
    data13=list(data13a,data13b,data13c,data13d,data13e,data13f,data13g)
    data14=list(data14a,data14b,data14c,data14d,data14e,data14f,data14g)
    data15=list(data15a,data15b,data15c,data15d,data15e,data15f,data15g)
    data16=list(data16a,data16b,data16c,data16d,data16e,data16f,data16g)
    
    data17=list(data17a,data17b,data17c)
    data18=list(data18a,data18b,data18c)
    data19=list(data19a,data19b,data19c)
    data20=list(data20a,data20b,data20c)
    data21=list(data21a,data21b,data21c)
    data22=list(data22a,data22b,data22c)
    data23=list(data23a,data23b,data23c)
    data24=list(data24a,data24b,data24c)
    data25=list(data25a,data25b,data25c)
    data26=list(data26a,data26b,data26c)
    
    data27=list(data27a)
    data28=list(data28a)
    data29=list(data29a)
    data30=list(data30a)
    data31=list(data31a)
    
    data_lmi=list(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,
                  data11,data12,data13,data14,data15,data16,data17,data18,data19,data20,
                  data21,data22,data23,data24,data25,data26,data27,data28,data29,data30,data31)
  }
  return(data_lmi)
}