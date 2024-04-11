#' TMB helper function. Extract parameter from `obj` to use as initial
#'  values in optimization
#'
#' @details The code was copied from a non-exported function in the
#'  `TMBhelper` package called `extract_fixed()`
#'
#' @param obj Object to be used as input to [TMB::MakeADFun()]
#'
#' @return A vector of initial values of parameters
#'
#' @export
extract_initial_values <- function(obj){

  if(!length(obj$env$random)) {
    obj$env$last.par.best
  }
  else {
    obj$env$last.par.best[-c(obj$env$random)]
  }
}

