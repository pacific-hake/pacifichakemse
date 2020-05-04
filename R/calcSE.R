#' Calculate the standard error on SSB between the Operating Model and the Estimation Model
#' for all runs in the list
#'
#' @param lst list of MSE results
#'
#' @return A [data.frame] of 3 columns: The run number, the standard error of spawning
#' stock biomass from EM in regards to the OM, and the year
#' @export
calc_standard_error_ssb <- function(lst){

  nruns <- length(lst)
  yrs <- lst[[1]]$SSB.hes$year
  nyear <- length(yrs)
  map_df(seq_along(lst), ~{
    if(is.na(.x)){
      NA
    }else{
      yrs <- lst[[.x]]$SSB.hes$year
      nyrs <- length(yrs)
      ssb_true <- rowSums(lst[[.x]]$SSB)
      ssb_est <- lst[[.x]]$SSB.hes$value
      err <- (ssb_est - ssb_true) / ssb_true
      data.frame(run = rep(.x, nyrs),
                 SE.SSB = err,
                 year = yrs)
    }
  })
}
