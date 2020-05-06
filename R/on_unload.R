#' Unload the dynamic library when the package is unloaded
#'
#' @param libpath Path where the library is located
#'
#' @return Nothing
#' @export
.onUnload <- function(libpath){
  library.dynam.unload("PacifichakeMSE", libpath)
}