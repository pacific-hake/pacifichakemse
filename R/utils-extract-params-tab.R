#' Extract parameter estimates from the linear par vector returned by TMB
#' to a list with parameter names and values
#'
#' @param obj The output from the TMB model
#'
#' @return A named [list] of parameter estimates extracted from `obj$par`
extract_params_tmb <- function(obj){

  par <- obj$par
  nms <- unique(names(par))

  map(nms, ~{
    par[names(par) == .x] |>
      unname()
  }) |>
    set_names(nms)
}

