#' Load the dynamic library when the package is loaded
#'
#' @param lib The name of the dynamic link library
#' @param pkg The package name
#'
#' @return Nothing
.onLoad <- function(lib, pkg) {
  #library.dynam("runHakeassessment", package = pkg, lib.loc = .libPaths())
  library.dynam("runHakeassessment", package = pkg, lib.loc = lib)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Loading runHakeassessment dynamic link (shared object) library...\n",
                        "Pass DLL = 'runHakeassessment' to all 'MakeADFun()' calls.")
}