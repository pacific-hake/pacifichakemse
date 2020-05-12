#' Load the dynamic library when the package is loaded
#'
#' @param lib The name of the dynamic link library
#' @param pkg The package name
#'
#' @return Nothing
.onLoad <- function(lib, pkg) {
  message("Loading runHakeassessment dynamic link (shared object) library...")
  message("Pass DLL = 'runHakeassessment' to all 'MakeADFun()' calls.")
  message("When running devtools::document() on this package, you must edit",
          "the dynamic loading in R/on_load.R")
  # TODO: When running devtools::document(), the following method must be used:
  # lib_path <- file.path("src",
  #                       TMB::dynlib("runHakeassessment"))
  # dyn.load(lib_path)
  #
  # When running devtools::install(), the following method must be used:
  library.dynam("runHakeassessment", pkg, lib)
}