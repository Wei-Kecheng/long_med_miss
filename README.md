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
library(MASS)
library(glmnet) #lasso
library(softImpute) #matrix completion for imputation
library(hdmed) #Pathway lasso mediation
library(hdi) #de-biased lasso
library(HDMT) #FDR control mediation
library(bama) #Bayesian mediation
n=350 #sample size
p=50 #dimension of mediators
t=4 #time
rho1=0.5
rho2=0.1
#exposure
x1=runif(n)
x2=x1+runif(n)
x3=x2+runif(n)
x4=x3+runif(n)
x=c(rbind(x1,x2,x3,x4))
#data frame
data=matrix(NA,n*t,p+5)
data[,p+1]=c(t(x))
data[,p+3]=rep(1:n,each=t)
data[,p+4]=rep(1:t,n)
#residuals#######################################################################
b=10;b1=p/b
em=c()
for (j in 1:b) {
  
  e=mvrnorm(n,rep(0,b1*t),matrix(rho1,b1*t,b1*t)+diag(1-rho1,b1*t,b1*t))
  for (i in 1:b1) {
    em=cbind(em,c(t(e[,((i-1)*t+1):(i*t)])))
  }
}
e=mvrnorm(n,rep(0,t),matrix(rho2,t,t)+diag(1-rho2,t,t))
ey=c(t(e))
#regression coefficients##########################################################
AA=c(0.5,0.5,1,1)

BB=matrix(0,p,t)
BB[1,]=c(1,1,1,1)
BB[2,]=c(1,1,1,1)
BB[3,]=c(1,2,3,4)
BB[4,]=c(4,3,2,1)
BB[5,]=c(1,1,0,0)
BB[6,]=c(1,0,1,0)
BB[7,]=c(1,0,0,1)
BB[8,]=c(0,1,1,0)
BB[9,]=c(0,1,0,1)
BB[10,]=c(0,0,1,1)
BB[11,]=c(1,2,3,4)
BB[12,]=c(4,3,2,1)
BB=BB*(-0.5)

GG=matrix(0,p,t)
GG[1,]=c(1,1,1,1)
GG[2,]=c(1,1,1,1)
GG[3,]=c(1,2,3,4)
GG[4,]=c(4,3,2,1)
GG[5,]=c(1,1,0,0)
GG[6,]=c(1,0,1,0)
GG[7,]=c(1,0,0,1)
GG[8,]=c(0,1,1,0)
GG[9,]=c(0,1,0,1)
GG[10,]=c(0,0,1,1)
GG[13,]=c(1,2,3,4)
GG[14,]=c(4,3,2,1)
GG=GG*(0.5)

DD=0
TT=rep(0,p)
parT=c(c(rbind(AA,BB,GG)),DD,TT)
#mediator+outcome####################################################################
for (j in 1:t) {
  
  sub=which(data[,p+4]==j)
  data[sub,1:p]=cbind(data[sub,p+1],1)%*%rbind(GG[,j],TT)+em[sub,]
  data[sub,p+2]=cbind(data[sub,c(p+1,1:p)],1)%*%c(AA[j],BB[,j],DD)+ey[sub]
}
#missing at random####################################################################
ind=1:n
len=c(50,100,100,100)
pro=rowSums(abs(matrix(data[,p+1],nrow=n,byrow=T)))
for (i in c(1,2,3,4)) {
  
  sub=sample(ind,len[i],prob=pro[ind])
  data[which(data[,p+3] %in% sub),p+5]=i
  
  ind=setdiff(ind,sub)
}
data[which(data[,p+5]==2 & data[,p+4]%in%c(4)),1:p]=NA
data[which(data[,p+5]==3 & data[,p+4]%in%c(1,2)),1:p]=NA
data[which(data[,p+5]==4 & data[,p+4]%in%c(1,2,4)),1:p]=NA
#longitudinal multiple imputation#####################################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/simulation/LMI.R"))
data_lmi=LMI(data,p,t,patt="A")
data_cc=data_lmi[[1]][[1]] #complete-case analysis
#matrix completion####################################################################
fit=softImpute(data[,1:(p+2)],rank=min(dim(data[,1:(p+2)]))-1)
data_soft=cbind(complete(data[,1:(p+2)],fit),data[,(p+3):(p+5)])
#single imputation####################################################################
miss=list(c(1,2,3,4),c(1,2,3),c(3,4),3)
for (i in 2:length(miss)) {
  
  sub=data[which(data[,p+5]==i & data[,p+4]%in%miss[[i]]),]
  ave=rowsum(sub[,1:p],sub[,p+3],reorder=FALSE)/length(miss[[i]])
  for (j in setdiff(1:t,miss[[i]])) { data[which(data[,p+5]==i & data[,p+4]%in%c(j)),1:p]=ave }
}
data_si=data
#GMM_PCA#################################################################################
p=50;t=4;l1=0.1;l2=0.01;patt="A"
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/simulation/GMM_PCA.R"))
theta1=GMM_PCA(data=data_lmi,p=p,t=t,lam1=l1,lam2=l2,patt=patt)
#tuning
lam1_seq=seq(0.01,0.1,length.out=5)
lam2_seq=seq(0.001,0.01,length.out=5)
grid=expand.grid(lam1=lam1_seq,lam2=lam2_seq)
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/simulation/bic.R"))
result=apply(grid,1,function(params){
  theta=GMM_PCA(data=data_lmi,p=p,t=t,lam1=params["lam1"],lam2=params["lam2"],patt=patt)
  bic_value=bic(data=data_lmi,theta=theta,p=p,t=t)
  list(theta=theta,bic=bic_value)})
theta1=result[[which.min(sapply(result,`[[`,"bic"))]]$theta
#complete-case analysis###################################################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/simulation/CC.R"))
theta2=CC(data=data_cc,p=p,t=t,lam1=l1,lam2=l2)
theta3=CC(data=data_soft,p=p,t=t,lam1=l1,lam2=l2)
theta4=CC(data=data_si,p=p,t=t,lam1=l1,lam2=l2)
#cross-sectional analysis#################################################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/simulation/CS.R"))
theta5=CS(data=data_cc,p=p,t=t,lam1=l1)
theta6=CS(data=data_soft,p=p,t=t,lam1=l1)
theta7=CS(data=data_si,p=p,t=t,lam1=l1)
#mixed-effects models#####################################################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/simulation/MIX.R"))
theta8=MIX(data=data_cc,p=p,t=t)
theta9=MIX(data=data_soft,p=p,t=t)
theta10=MIX(data=data_si,p=p,t=t)
#pathway lasso############################################################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/simulation/PATH.R"))
theta11=PATH(data=data_cc,p=p,t=t,tun=F,L=l1/10,R=l1,lam=l1)
theta12=PATH(data=data_soft,p=p,t=t,tun=F,L=l1/10,R=l1,lam=l1)
theta13=PATH(data=data_si,p=p,t=t,tun=F,L=l1/10,R=l1,lam=l1)
#de-biased lasso and false discovery rate control#########################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/simulation/HIMA.R"))
theta14=HIMA(data=data_cc,p=p,t=t)
theta15=HIMA(data=data_soft,p=p,t=t)
theta16=HIMA(data=data_si,p=p,t=t)
#Bayesian sparse models###################################################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/simulation/BAY.R"))
theta17=BAY(data=data_cc,p=p,t=t)
theta18=BAY(data=data_soft,p=p,t=t)
theta19=BAY(data=data_si,p=p,t=t)
```
