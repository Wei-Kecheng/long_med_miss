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
library(ranger) #random forests
library(glmnet) #lasso
n=200 #sample size
p=100 #dimension of mediators
q=10 #dimension of confounders
#nonparametric confounding effects###########################################
z=matrix(runif(n*q,-1,1),ncol=q)
h=z[,1]+z[,2]^2+4*(z[,3]>0)
g=h
f=h
#true coeffcients############################################################
alpha=1
gamma=c(1,1,1,1,1,1,0,0,rep(0,p-8))*(-1)
beta=c(1,1,1,1,0,0,1,1,rep(0,p-8))*(-1)
#exposure+mediators+outcome###################################################
x=h+rnorm(n)
m=as.matrix(x)%*%gamma+matrix(rep(g,p),ncol=p)+matrix(rnorm(n*p),ncol=p)
y=x*alpha+m%*%beta+f+rnorm(n)
##############################################################################
source("ML_SS_ranger.R")
source("DML_HDMA.R")
result=DML_HDMA(x,m,y,z,K=5)
