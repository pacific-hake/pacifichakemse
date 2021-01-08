# pacifichakemse

 <!-- badges: start -->
  [![R-CMD-check](https://github.com/pacific-hake/pacifichakemse/workflows/R-CMD-check/badge.svg)](https://github.com/pacific-hake/pacifichakemse/actions)
  <!-- badges: end -->
  
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Coverage status](https://codecov.io/gh/pacific-hake/pacifichakemse/branch/master/graph/badge.svg)](https://codecov.io/github/pacific-hake/pacifichakemse?branch=master)

Management Strategy Evaluation (MSE) for Pacific hake

To use this package, you must have [Rtools 4](https://cran.r-project.org/bin/windows/Rtools/) installed to allow compilation of the Estimation Model which is coded in C++ using the [TMB](https://github.com/kaskr/adcomp) package.

To get started, clone or download this repository and the [companion project](https://github.com/pacific-hake/runhakemse) which contains the calls to the package functions necessary for running the MSE and plotting the results.

This package originated from the [Pacific hake MSE written by Nis Sand Jacobsen](https://github.com/nissandjac/PacifichakeMSE). The [Estimation Model](https://github.com/pacific-hake/pacifichakemse/blob/master/src/pacifichakemse.cpp) was entirely written by him and has not been modified in this version. This package diverged from that one enough that we chose to continue it as a separate package instead of as a branch of the original project. This is reflected in the commit history and network graph of this repository.