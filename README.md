# Simulation codes
------------------------------------------------
# Description
- LMI: Performs longitudinal multiple imputation.
- GMM_PCA: Implements the generalized method of moments with principal component analysis to estimate parameters.
- CS: Conducts cross-sectional mediation analysis following the method proposed by Xue et al. (2022).
- MIX: Fits mixed-effects models with time interactions and lasso regularization, based on Rijnhart et al. (2022).
- Path: Implements linear structural equation modeling with pathway lasso penalty, as described in Zhao and Luo (2022).
- HIMA: Performs high-dimensional mediation analysis using de-biased lasso and false discovery rate control, following Perera et al. (2022).
- Bayes: Applies Bayesian sparse modeling with continuous shrinkage priors, based on Song et al. (2020).

# Usage
- LMI (data,p,t)
- GMM_PCA (data,p,t,lam1,lam2,a=3.7,c=10,patt)
  
# Arguments
LMI(data,p,t)
- data: A data frame with nt rows and p+5 columns, representing n samples observed over multiple time points. Each row corresponds to an observation at a specific time point. Columns 1 to p: mediators. Column p+1: exposure. Column p+2: outcome. Column p+3: sample ID. Column p+4: time ID. Column p+5: missingness group ID. Missing values are denoted as NA.
- p: Number of mediators.
- t: Number of time points.
- patt: Specifies different non-monotone missingness structures.

GMM_PCA(data,p,t,lam1,lam2,a=3.7,c=10)
- data: A list containing multiple imputed datasets.
- p: Number of mediators.
- t: Number of time points.
- lam1: Regularization parameter for the mediation effect penalty.
- lam2: Regularization parameter for the time-varying effect penalty.
- a: Tuning parameter for the SCAD penalty, with a default value of 3.7.
- c: Tuning parameter for the bi-level penalty, with a default value of 10.
- patt: Specifies different non-monotone missingness structures.

# Value
LMI(data,p,t)
- Returns a list of multiple imputed datasets.

GMM_PCA (data,p,t,lam1,lam2,a=3.7,c=10,patt)
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
#residuals#####################################################################
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
#longitudinal multiple imputation######################################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/simulation/LMI.R"))
data_lmi=LMI(data,p,t,patt="A")
data_cc=data_lmi[[1]][[1]] #complete-case analysis
#matrix completion######################################################################
fit=softImpute(data[,1:(p+2)],rank=min(dim(data[,1:(p+2)]))-1)
data_soft=cbind(complete(data[,1:(p+2)],fit),data[,(p+3):(p+5)])
#single imputation#####################################################################
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
#mixed-effects models######################################################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/simulation/MIX.R"))
theta8=MIX(data=data_cc,p=p,t=t)
theta9=MIX(data=data_soft,p=p,t=t)
theta10=MIX(data=data_si,p=p,t=t)
#pathway lasso##############################################################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/simulation/PATH.R"))
theta11=PATH(data=data_cc,p=p,t=t,tun=F,L=l1/10,R=l1,lam=l1)
theta12=PATH(data=data_soft,p=p,t=t,tun=F,L=l1/10,R=l1,lam=l1)
theta13=PATH(data=data_si,p=p,t=t,tun=F,L=l1/10,R=l1,lam=l1)
#de-biased lasso and false discovery rate control##########################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/simulation/HIMA.R"))
theta14=HIMA(data=data_cc,p=p,t=t)
theta15=HIMA(data=data_soft,p=p,t=t)
theta16=HIMA(data=data_si,p=p,t=t)
#Bayesian sparse models####################################################################
source(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/simulation/BAY.R"))
theta17=BAY(data=data_cc,p=p,t=t)
theta18=BAY(data=data_soft,p=p,t=t)
theta19=BAY(data=data_si,p=p,t=t)
```

# References
- XUE, F., TANG, X., KIM, G., KOENEN, K. C., MARTIN, C. L., GALEA, S., WILDMAN, D., UDDIN, M. and QU, A. (2022). Heterogeneous mediation analysis on epigenomic ptsd and traumatic stress in a predominantly
African American cohort. J. Am. Stat. Assoc. 117 1669–1683.
- RIJNHART, J. J. M., TWISK, J. W. R., VALENTE, M. J. and HEYMANS, M. W. (2022). Time lags and time interactions in mixed effects models impacted longitudinal mediation effect estimates. J. Clin. Epidemiol. 151 143–150.
- ZHAO, Y. and LUO, X. (2022). Pathway lasso: Pathway estimation and selection with high-dimensional mediators. Stat. Its Interface 15 39–50.
- PERERA, C., ZHANG, H., ZHENG, Y., HOU, L., QU, A., ZHENG, C., XIE, K. and LIU, L. (2022). HIMA2: High-dimensional mediation analysis and its application in epigenome-wide DNA methylation data. BMC Bioinform. 23 296.
- SONG, Y., ZHOU, X., ZHANG, M., ZHAO, W., LIU, Y., KARDIA, S. L. R., DIEZ ROUX, A. V., NEEDHAM, B. L., SMITH, J. A. and MUKHERJEE, B. (2020). Bayesian shrinkage estimation of high dimensional causal mediation effects in omics studies. Biometrics 76 700–710.
