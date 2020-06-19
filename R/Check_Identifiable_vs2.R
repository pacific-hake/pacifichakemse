#' Calculate the estimated parameters with uncertainty
#'
#' @param obj Object to be used as input to [TMB::MakeADFun()]
#'
#' @return Return a list of estimated parameters and uncertainty
#' @importFrom stats optimHess
#' @export
Check_Identifiable_vs2 <- function(obj){
  # Finite-different hessian
  par_hat <- extract_initial_values(obj)
  lst <- NULL
  lst$hess <- optimHess(par = par_hat,
                        fn = obj$fn,
                        gr = obj$gr)

  # Check eigendecomposition
  if(is.nan(max(lst$hess))){
    print("model not converging")
  }else{
    lst$eigen <- eigen(lst$hess)
    lst$which_bad <- which(lst$eigen$values < sqrt(.Machine$double.eps))

    # Check for parameters
    if(length(lst$eigen$vectors[, lst$which_bad]) > 0){
      row_max = apply(as.matrix(lst$eigen$vectors[, lst$which_bad]),
                      MARGIN = 1,
                      FUN = function(vec){max(abs(vec))})
    }else{
      row_max <- rep(0,length(lst$eigen$values))
    }

    browser()

    lst$bad_params <- data.frame("Param" = names(obj$par),
                                 "MLE" = par_hat,
                                 "Param_check" = ifelse(row_max > 0.1, "Bad", "OK"))

    # Message
    if(length(lst$which_bad) == 0){
      message("All parameters are identifiable")
    }else{
      print(lst$bad_params)
    }
  }
  lst
}
