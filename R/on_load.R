#' Load the dynamic library when the package is loaded
#'
#' @param lib The name of the dynamic link library
#' @param pkg The package name
#'
#' @return Nothing
.onLoad <- function(lib, pkg) {
  message("Loading runHakeassessment dynamic link (shared object) library...")
  message("Pass DLL = 'runHakeassessment' to all 'MakeADFun()' calls.")
  library.dynam("runHakeassessment", package = pkg, lib.loc = .libPaths())
}