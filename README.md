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

# References
- XUE, F., TANG, X., KIM, G., KOENEN, K. C., MARTIN, C. L., GALEA, S., WILDMAN, D., UDDIN, M. and QU, A. (2022). Heterogeneous mediation analysis on epigenomic ptsd and traumatic stress in a predominantly
African American cohort. J. Am. Stat. Assoc. 117 1669–1683.
- RIJNHART, J. J. M., TWISK, J. W. R., VALENTE, M. J. and HEYMANS, M. W. (2022). Time lags and time interactions in mixed effects models impacted longitudinal mediation effect estimates. J. Clin. Epidemiol. 151 143–150.
- ZHAO, Y. and LUO, X. (2022). Pathway lasso: Pathway estimation and selection with high-dimensional mediators. Stat. Its Interface 15 39–50.
- PERERA, C., ZHANG, H., ZHENG, Y., HOU, L., QU, A., ZHENG, C., XIE, K. and LIU, L. (2022). HIMA2: High-dimensional mediation analysis and its application in epigenome-wide DNA methylation data. BMC Bioinform. 23 296.
- SONG, Y., ZHOU, X., ZHANG, M., ZHAO, W., LIU, Y., KARDIA, S. L. R., DIEZ ROUX, A. V., NEEDHAM, B. L., SMITH, J. A. and MUKHERJEE, B. (2020). Bayesian shrinkage estimation of high dimensional causal mediation effects in omics studies. Biometrics 76 700–710.
