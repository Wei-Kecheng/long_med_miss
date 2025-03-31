# Application codes
------------------------------------------------
# Description
Data for application can be requested from the Detroit Neighborhood Health Study (DNHS) website: https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id=phs000560.v2.p1

# Usage
- `LMI(data,p,t,patt)`
- `GMM_PCA(data,p,t,lam1,lam2,a=3.7,c=10,patt)`
- `CC=function(data,p,t,lam1,lam2,a=3.7,c=10)`
- `CS=function(data,p,t,lam1,a=3.7,c=10)`
- `MIX(data,p,t)`
- `PATH(data,p,t,tun,L,R,lam)`
- `HIMA(data,p,t)`
- `BAY(data,p,t)`

  
# Arguments
`LMI(data,p,t,patt)`
- data: A data frame with nt rows and p+5 columns, representing n samples observed over t time points. Each row corresponds to an observation at a specific time point. Columns 1 to p: mediators. Column p+1: exposure. Column p+2: outcome. Column p+3: sample ID. Column p+4: time ID. Column p+5: missingness group ID. Missing values are denoted as NA.
- p: Number of mediators.
- t: Number of time points.
- patt: Specifies different non-monotone missingness structures.

`GMM_PCA(data,p,t,lam1,lam2,a=3.7,c=10,patt)`
- data: A list containing multiple imputed datasets.
- p: Number of mediators.
- t: Number of time points.
- lam1: Regularization parameter for the mediation effect penalty.
- lam2: Regularization parameter for the time-varying effect penalty.
- a: Tuning parameter for the SCAD penalty, with a default value of 3.7.
- c: Tuning parameter for the bi-level penalty, with a default value of 10.
- patt: Specifies different non-monotone missingness structures.

` CC=function(data,p,t,lam1,lam2,a=3.7,c=10)`
- data: An imputed dataset without missingness.
- p: Number of mediators.
- t: Number of time points.
- lam1: Regularization parameter for the mediation effect penalty.
- lam2: Regularization parameter for the time-varying effect penalty.
- a: Tuning parameter for the SCAD penalty, with a default value of 3.7.
- c: Tuning parameter for the bi-level penalty, with a default value of 10.

` CS=function(data,p,t,lam1,a=3.7,c=10)`
- data: An imputed dataset without missingness.
- p: Number of mediators.
- t: Number of time points.
- lam1: Regularization parameter for the mediation effect penalty.
- a: Tuning parameter for the SCAD penalty, with a default value of 3.7.
- c: Tuning parameter for the bi-level penalty, with a default value of 10.

`MIX(data,p,t)`
- data: An imputed dataset without missingness.
- p: Number of mediators.
- t: Number of time points.

`PATH(data,p,t,tun,L,R,lam)`
- data: An imputed dataset without missingness.
- p: Number of mediators.
- t: Number of time points.
- tun: Logical flag indicating whether to perform regularization parameter selection using the stability criterion. If TRUE, a grid search is conducted within the range [L,R]. If FALSE, the function runs with the specified regularization parameter lam.
- L: Left bound of the regularization parameter search range.
- R: Right bound of the regularization parameter search range.
- lam: Regularization parameter for the mediation effect penalty.

`HIMA(data,p,t)`
- data: An imputed dataset without missingness.
- p: Number of mediators.
- t: Number of time points.

`BAY(data,p,t)`
- data: An imputed dataset without missingness.
- p: Number of mediators.
- t: Number of time points.

# Value
`LMI(data,p,t,patt)`
- Returns a list of multiple imputed datasets.

`GMM_PCA(data,p,t,lam1,lam2,a=3.7,c=10,patt)`
`CC=function(data,p,t,lam1,lam2,a=3.7,c=10)`
`CS=function(data,p,t,lam1,a=3.7,c=10)`
`MIX(data,p,t)`
`PATH(data,p,t,tun,L,R,lam)`
`HIMA(data,p,t)`
`BAY(data,p,t)`
- Returns a vector of estimated coefficients.

# Examples
```r
library(glmnet) #lasso
library(softImpute) #matrix completion for imputation
library(hdmed) #Pathway lasso mediation
library(hdi) #de-biased lasso
library(HDMT) #FDR control mediation
library(bama) #Bayesian mediation
p=1879;q=8;t=5
data=read.csv(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/application/data_origin1.csv"))
#data clean##################################################################################################
na1=which(is.na(data$smoke))
na2=which(is.na(data$race6cat))
na3=which(is.na(data$female))
na4=which(is.na(data$Social_Cohesion))
na5=which(is.na(data$w1_educ)&
            is.na(data$w2_educ)&
            is.na(data$w3_educ)&
            is.na(data$w4_educ)&
            is.na(data$w5_educ))
na6=which(is.na(data$w1_age_final)&
            is.na(data$w2_age_final)&
            is.na(data$w3_age_final)&
            is.na(data$w4_age_final)&
            is.na(data$w5_age_final))
na7=which(is.na(data$w1c1_U10)&
            is.na(data$w2c1_U10)&
            is.na(data$w2c2_U10)&
            is.na(data$w3_U10)&
            is.na(data$w4_U10)&
            is.na(data$w5_U10))
na8=which(is.na(data$w1c1_phq9cat)&
            is.na(data$w2c1_pyphq9cat)&
            is.na(data$w2c2_phq9cat)&
            is.na(data$w3_slphq9cat)&
            is.na(data$w4_slphq9cat)&
            is.na(data$w5_slphq9cat))
na9=which(is.na(data$w1c1_life_sumptsdworst)&
            is.na(data$w2c1_life_sumptsdworst)&
            is.na(data$w2c2_life_sumptsdworst)&
            is.na(data$w3_life_sumptsdworst)&
            is.na(data$w4_life_sumptsdworst)&
            is.na(data$w5_life_sumptsdworst))
na10=which(is.na(data$w1c1_traumanum)&
             is.na(data$w2c1_traumanum)&
             is.na(data$w2c2_traumanum)&
             is.na(data$w3_traumanum)&
             is.na(data$w4_traumanum)&
             is.na(data$w5_traumanum))
data=data[-Reduce(union,list(na1,na2,na3,na4,na5,na6,na7,na8,na9,na10)),]
#############################################################################################################
imp1=function(u){
  
  len=which(!is.na(u))[1]
  return(u[len]-(len-1))
}
imp2=function(u){
  
  v=c()
  for (j in 1:t) {
    
    len=which(!is.na(u))
    len=len[which.min(abs(len-j))]
    v[j]=u[len]
  }
  return(v)
}
na=function(u){
  if(sum(is.na(u))==2){
    return(NA)
  }else{
    return(na.omit(u))
  }
}
###############################################################################################################
mi=which(colnames(data) %in% c('w1c1_traumanum','w2c1_traumanum','w2c2_traumanum','w3_traumanum','w4_traumanum','w5_traumanum'))
data[,mi][is.na(data[,mi])]=0

name=c(colnames(data[,1:p]),"z1","z2","z3","z4","z5","z6","z7","z8","x","y","RESP","wave","g")
new=data.frame(matrix(ncol=length(name),nrow=0))
colnames(new)=name
#data reshape###################################################################################################
id=unique(data$RESP)
for (i in 1:length(id)) {
  
  mid=data[which(data$RESP==id[i]),]
  mid=mid[order(mid$wave),]
  fir=mid[1,]
  
  wave=c(1:t)
  RESP=rep(fir$RESP,t)
  z1=rep(fir$smoke,t)
  z2=rep(fir$race6cat==2,t)
  z3=rep(fir$female,t)
  
  z4=na.omit(c(fir$w1_educ,fir$w2_educ,fir$w3_educ,fir$w4_educ,fir$w5_educ))[1]
  z4=rep(z4,t)
  
  z5=na.omit(c(fir$w1c1_U10,fir$w2c1_U10,fir$w2c2_U10,fir$w3_U10,fir$w4_U10,fir$w5_U10))[1]
  z5=rep(z5,t)
  
  z6=na.omit(c(fir$w1c1_phq9cat,fir$w2c1_pyphq9cat,fir$w2c2_phq9cat,fir$w3_slphq9cat,fir$w4_slphq9cat,fir$w5_slphq9cat))[1]
  z6=rep(z6,t)
  
  z7=c(fir$w1_age_final,fir$w2_age_final,fir$w3_age_final,fir$w4_age_final,fir$w5_age_final)
  z7=rep(imp1(z7),t)
  
  z8=rep(fir$Social_Cohesion,t)
  ########################################################################################################################
  y=c(fir$w1c1_life_sumptsdworst,na(c(fir$w2c1_life_sumptsdworst,fir$w2c2_life_sumptsdworst)),fir$w3_life_sumptsdworst,fir$w4_life_sumptsdworst,fir$w5_life_sumptsdworst)
  y=imp2(y)
  
  x=c(fir$w1c1_traumanum,
      fir$w1c1_traumanum+fir$w2c1_traumanum+fir$w2c2_traumanum,
      fir$w1c1_traumanum+fir$w2c1_traumanum+fir$w2c2_traumanum+fir$w3_traumanum,
      fir$w1c1_traumanum+fir$w2c1_traumanum+fir$w2c2_traumanum+fir$w3_traumanum+fir$w4_traumanum,
      fir$w1c1_traumanum+fir$w2c1_traumanum+fir$w2c2_traumanum+fir$w3_traumanum+fir$w4_traumanum+fir$w5_traumanum)
  ##########################################################################################################################
  if(nrow(mid)==4){
    cpg=rbind(mid[1,1:p],mid[2,1:p],NA,mid[3,1:p],mid[4,1:p]);g=rep(1,t)
  }
  
  else if(nrow(mid)==3 & mid$wave[1]=="w1c1_bloodid" & mid$wave[2]=="w2_bloodid" & mid$wave[3]=="w4_bloodid"){
    cpg=rbind(mid[1,1:p],mid[2,1:p],NA,mid[3,1:p],NA);g=rep(2,t)
  }
  else if(nrow(mid)==3 & mid$wave[1]=="w1c1_bloodid" & mid$wave[2]=="w2_bloodid" & mid$wave[3]=="w5_bloodid"){
    cpg=rbind(mid[1,1:p],mid[2,1:p],NA,NA,mid[3,1:p]);g=rep(3,t)
  }
  else if(nrow(mid)==3 & mid$wave[1]=="w1c1_bloodid" & mid$wave[2]=="w4_bloodid" & mid$wave[3]=="w5_bloodid"){
    cpg=rbind(mid[1,1:p],NA,NA,mid[2,1:p],mid[3,1:p]);g=rep(4,t)
  }
  else if(nrow(mid)==3 & mid$wave[1]=="w2_bloodid" & mid$wave[2]=="w4_bloodid" & mid$wave[3]=="w5_bloodid"){
    cpg=rbind(NA,mid[1,1:p],NA,mid[2,1:p],mid[3,1:p]);g=rep(5,t)
  }
  
  else if(nrow(mid)==2 & mid$wave[1]=="w1c1_bloodid" & mid$wave[2]=="w2_bloodid"){
    cpg=rbind(mid[1,1:p],mid[2,1:p],NA,NA,NA);g=rep(6,t)
  }
  else if(nrow(mid)==2 & mid$wave[1]=="w1c1_bloodid" & mid$wave[2]=="w4_bloodid"){
    cpg=rbind(mid[1,1:p],NA,NA,mid[2,1:p],NA);g=rep(7,t)
  }
  else if(nrow(mid)==2 & mid$wave[1]=="w1c1_bloodid" & mid$wave[2]=="w5_bloodid"){
    cpg=rbind(mid[1,1:p],NA,NA,NA,mid[2,1:p]);g=rep(8,t)
  }
  else if(nrow(mid)==2 & mid$wave[1]=="w2_bloodid" & mid$wave[2]=="w4_bloodid"){
    cpg=rbind(NA,mid[1,1:p],NA,mid[2,1:p],NA);g=rep(9,t)
  }
  else if(nrow(mid)==2 & mid$wave[1]=="w2_bloodid" & mid$wave[2]=="w5_bloodid"){
    cpg=rbind(NA,mid[1,1:p],NA,NA,mid[2,1:p]);g=rep(10,t)
  }
  else if(nrow(mid)==2 & mid$wave[1]=="w4_bloodid" & mid$wave[2]=="w5_bloodid"){
    cpg=rbind(NA,NA,NA,mid[1,1:p],mid[2,1:p]);g=rep(11,t)
  }
  
  else if(nrow(mid)==1 & mid$wave[1]=="w1c1_bloodid"){
    cpg=rbind(mid[,1:p],NA,NA,NA,NA);g=rep(12,t)
  }
  else if(nrow(mid)==1 & mid$wave[1]=="w2_bloodid"){
    cpg=rbind(NA,mid[,1:p],NA,NA,NA);g=rep(13,t)
  }
  else if(nrow(mid)==1 & mid$wave[1]=="w4_bloodid"){
    cpg=rbind(NA,NA,NA,mid[,1:p],NA);g=rep(14,t)
  }
  else if(nrow(mid)==1 & mid$wave[1]=="w5_bloodid"){
    cpg=rbind(NA,NA,NA,NA,mid[,1:p]);g=rep(15,t)
  }
  new=rbind(new,cbind(cpg,z1,z2,z3,z4,z5,z6,z7,z8,x,y,RESP,wave,g))
}
#############################################################################################################
new=new[-which(new$wave==3),]
t=4
new$wave=rep(c(1:t),length(id))
#normalization################################################################################################
normal=function(u){ (u-min(u))/(max(u)-min(u)) }
new$z1=normal(new$z1)
new$z2=normal(new$z2)
new$z3=normal(new$z3)
new$z4=normal(new$z4)
new$z5=normal(new$z5)
new$z6=normal(new$z6)
new$z7=normal(new$z7)
new$z8=normal(new$z8)
new$x=normal(new$x)
new$y=log(new$y)
#residuals after adjustment############################################################################################
new$y=lm(y~z1+z2+z3+z4+z5+z6+z7+z8,data=new)$residuals
new=new[,-c((p+1):(p+q))]
#screening#############################################################################################################
data=matrix(unlist(new),nrow=nrow(new))
med=matrix(NA,t,p)
sn=c()

for (j in 1:t) {
  
  sub=na.omit(data[which(data[,p+4]==j),])
  sn[j]=floor(nrow(sub)/log(nrow(sub)))
  
  for (m in 1:p) {
    a=lm(sub[,p+2]~sub[,c(m,p+1)])$coefficients[2]
    b=lm(sub[,m]~sub[,p+1])$coefficients[2]
    med[j,m]=abs(a*b)
  }
}
a1=order(med[1,],decreasing=T)[1:sn[1]]
a2=order(med[2,],decreasing=T)[1:sn[2]]
a3=order(med[3,],decreasing=T)[1:sn[3]]
a4=order(med[4,],decreasing=T)[1:sn[4]]

sis=Reduce(union,list(a1,a2,a3,a4))
dna=name[sis]
data=cbind(data[,sis],data[,(p+1):ncol(data)]);p=length(dna)
#longitudinal multiple imputation#################################################################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/application/LMI.R"))
data_com=LMI(data,p,t)
data_cc=data_com[[1]][[1]][[1]]
#matrix completion################################################################################################
fit=softImpute(data[,1:(p+2)],rank=min(dim(data[,1:(p+2)]))-1)
data_soft=cbind(complete(data[,1:(p+2)],fit),data[,(p+3):(p+5)])
#single imputation#################################################################################################
miss=list(c(1,2,3,4),c(1,2,3),c(1,2,4),c(1,3,4),c(2,3,4),
          c(1,2),c(1,3),c(1,4),c(2,3),c(2,4),c(3,4),1,2,3,4)
for (i in 2:length(miss)) {
  
  sub=data[which(data[,p+5]==i & data[,p+4]%in%miss[[i]]),]
  ave=rowsum(sub[,1:p],sub[,p+3],reorder=FALSE)/length(miss[[i]])
  
  for (j in setdiff(1:t,miss[[i]])) { data[which(data[,p+5]==i & data[,p+4]%in%c(j)),1:p]=ave }
}
data_si=data
#GMM_PCA##################################################################################
l1=0.0003;l2=0.0005
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/application/GMM_PCA.R"))
theta1=GMM_PCA(data_com=data_com,p=p,t=t,lam1=l1,lam2=l2)
#tuning
lam1_seq=seq(l1/10,l1,length.out=5)
lam2_seq=seq(l2/10,l2,length.out=5)
grid=expand.grid(lam1=lam1_seq,lam2=lam2_seq)
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/application/bic.R"))
result=apply(grid,1,function(params){
  theta=GMM_PCA(data=data_com,p=p,t=t,lam1=params["lam1"],lam2=params["lam2"])
  bic_value=bic(data=data_com[[1]],theta=theta,p=p,t=t)
  list(theta=theta,bic=bic_value)})
theta1=result[[which.min(sapply(result,`[[`,"bic"))]]$theta
#complete-case analysis###################################################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/application/CC.R"))
theta2=CC(data=data_cc,p=p,t=t,lam1=l1,lam2=l2)
theta3=CC(data=data_soft,p=p,t=t,lam1=l1,lam2=l2)
theta4=CC(data=data_si,p=p,t=t,lam1=l1,lam2=l2)
#cross-sectional analysis#################################################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/application/CS.R"))
theta5=CS(data=data_cc,p=p,t=t,lam1=l1)
theta6=CS(data=data_soft,p=p,t=t,lam1=l1)
theta7=CS(data=data_si,p=p,t=t,lam1=l1)
#mixed-effects models#####################################################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/application/MIX.R"))
theta8=MIX(data=data_cc,p=p,t=t)
theta9=MIX(data=data_soft,p=p,t=t)
theta10=MIX(data=data_si,p=p,t=t)
#pathway lasso############################################################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/application/PATH.R"))
theta11=PATH(data=data_cc,p=p,t=t,tun=F,L=l1/10,R=l1,lam=l1*100)
theta12=PATH(data=data_soft,p=p,t=t,tun=F,L=l1/10,R=l1,lam=l1*100)
theta13=PATH(data=data_si,p=p,t=t,tun=F,L=l1/10,R=l1,lam=l1*100)
#de-biased lasso and false discovery rate control#########################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/application/HIMA.R"))
theta14=HIMA(data=data_cc,p=p,t=t)
theta15=HIMA(data=data_soft,p=p,t=t)
theta16=HIMA(data=data_si,p=p,t=t)
#Bayesian sparse models####################################################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/application/BAY.R"))
theta17=BAY(data=data_cc,p=p,t=t)
theta18=BAY(data=data_soft,p=p,t=t)
theta19=BAY(data=data_si,p=p,t=t)
```
