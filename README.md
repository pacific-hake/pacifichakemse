# PacifichakeMSE

[![Travis build status](https://travis-ci.org/cgrandin/PacifichakeMSE.svg?branch=master)](https://travis-ci.org/cgrandin/PacifichakeMSE)

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

[![Coverage status](https://codecov.io/gh/cgrandin/PacifichakeMSE/branch/master/graph/badge.svg)](https://codecov.io/github/cgrandin/PacifichakeMSE?branch=master)

Management Strategy Evaluation (MSE) for Pacific hake

To install this package, run the following code. You need to have [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed to allow compilation of the
estimation model which is coded in C++ using the [TMB](https://github.com/kaskr/adcomp) package.

```r
devtools::install_github("cgrandin/PacifichakeMSE")
```

To get started, clone or download the [companion project](https://github.com/cgrandin/runhakemse) which contains the calls to the package functions necessary for running the MSE and plotting the results.

## Operating model
The MSE runs with a spatial operating model and the estimation model used in the hake assessment using the file run_MSE_all.R
The file runs six hake management strategy evaluations, with 3 different harvest control rules, and 3 different movement rates. Run the file 'compare_MSE.R'to plot the results of the MSEs

## Hake stock assessment
Besides the MSE, the repository also contains the hake stock assessment rewritten in TMB. The assessment can be run from the file runHakeassessment.R

# Description
Detailed technical description is in progress.

### Disclaimer
All code by Nis Sand Jacobsen nissandjac@gmail.com . Please do not use in specific management scenarios without first consulting the author.  
