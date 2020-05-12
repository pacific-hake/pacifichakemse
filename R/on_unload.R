#' Unload the dynamic library when the package is unloaded
#'
#' @param libpath Path where the library is located
#'
#' @return Nothing
.onUnload <- function(libpath){
  message("Unloading runHakeassessment dynamic link (shared object) library...")
  try(library.dynam.unload("runHakeassessment", libpath), silent = TRUE)
}