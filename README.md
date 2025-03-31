# Time-varying mediation analysis for incomplete data with application to DNA methylation study for PTSD
----------------------------------------------------
# Description
This repository provides code for longitudinal mediation analysis with missing data (`long_med_miss`), including implementations for both simulation and application. 

# R functions
- `LMI`: Implements the proposed longitudinal multiple imputation method.
- `GMM_PCA`: Applies the generalized method of moments with principal component analysis.
- `CS`: Conducts cross-sectional mediation analysis following Xue et al. (2022).
- `MIX`: Fits mixed-effects models with time interactions and lasso regularization, based on Rijnhart et al. (2022).
- `Path`: Implements linear structural equation modeling with pathway lasso penalty, as described in Zhao and Luo (2022).
- `HIMA`: Performs high-dimensional mediation analysis using de-biased lasso and false discovery rate control, following Perera et al. (2022).
- `Bayes`: Applies Bayesian sparse modeling with continuous shrinkage priors, based on Song et al. (2020).
