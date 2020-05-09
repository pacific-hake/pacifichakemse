#' Unload the dynamic library when the package is unloaded
#'
#' @param libpath Path where the library is located
#'
#' @return Nothing
.onUnload <- function(libpath){
  message("Unloading PacifihakeMSE dynamic link (shared object) library...")
  library.dynam.unload("PacifichakeMSE", libpath)
}