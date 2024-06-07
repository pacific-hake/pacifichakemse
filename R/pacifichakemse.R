#' Pacific Hake MSE package
#'
#' @name pacifichakemse
#' @description Management strategy evaluation framework with a Spatial
#'  Operating Model.
#'
#' All of these imports are in alphabetical order, both packages and
#' functions within them
#' @import ggplot2
#' @import hake
#' @import httr2
#' @import knitr
#' @import purrr
#' @import readr
#' @import stringr
#' @import tibble
#' @import tidyr
#' @import tidyselect
#' @import TMB
#' @rawNamespace import(dplyr, except = c(lag))
#' @rawNamespace import(gfutilities, except = c(f, firstup, get_os, number_to_word))
#' @rawNamespace import(kableExtra, except = c(group_rows))
#' @rawNamespace import(r4ss, except = c(profile))
#' @rawNamespace import(stats, except = c(filter))

#' @importFrom clisymbols symbol
#' @importFrom cowplot plot_grid
#' @importFrom crayon green red white yellow
#' @importFrom forcats fct_relevel
#' @importFrom furrr future_imap future_map2 furrr_options
#' @importFrom graphics plot points polygon
#' @importFrom grDevices colorRampPalette col2rgb rgb
#' @importFrom grid grid.draw
#' @importFrom here here
#' @importFrom httr HEAD
#' @importFrom PNWColors pnw_palette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom reshape2 melt
#' @importFrom rlang sym :=
#' @importFrom r4ss SS_output SS_readdat
#' @importFrom tictoc tic toc
#' @importFrom TMB sdreport MakeADFun
#' @importFrom utils read.csv read.table tail
#'
#' @docType _PACKAGE
#' @author Chris J. Grandin, Nis S. Jacobsen, Ian G. Taylor, Kristin Marshall,
#'  Aaron Berger
#' Package maintainer: Chris J. Grandin <chris.grandin@@gmail.com>
#' @references pacifichakemse on GitHub:
#'  \url{https://github.com/pacific-hake/pacifichakemse}
NULL

#' @useDynLib pacifichakemse, .registration = TRUE
NULL
