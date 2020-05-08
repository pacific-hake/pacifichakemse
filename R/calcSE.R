#' Calculate the standard error on SSB between the Operating Model and the Estimation Model
#' for all runs in the list
#'
#' @param lst list of MSE results
#' @param quants A vector of quantiles to pass to [stats::quantile()]
#' @param inc_mean Logical. Include the mean (as column `avg`)
#'
#' @return A [data.frame] of 3 columns: The run number, the standard error of spawning
#' stock biomass from EM in regards to the OM, and the year
#' @export
calc_standard_error_ssb <- function(lst,
                                    col = "SE.SSB",
                                    quants = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                    inc_mean = TRUE){
  nruns <- length(lst)
  yrs <- lst[[1]]$SSB.hes$year
  nyrs <- length(yrs)
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
  }) %>%
    group_by(year) %>%
    group_map(~ calc_quantiles(.x, col = col, probs = quants, include_mean = inc_mean)) %>%
    map_df(~{.x}) %>%
    mutate(year = yrs) %>%
    select(year, everything())
}
