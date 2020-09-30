#' Calculate the standard error on SSB between the Operating Model and the Estimation Model
#' for all runs in the list
#'
#' @param lst list of MSE results
#' @param col Column to calculate the quantiles on
#' @param quants A vector of quantiles to pass to [stats::quantile()]
#' @param inc_mean Logical. Include the mean (as column `avg`)
#'
#' @return A [data.frame] of 3 columns: The run number, the standard error of spawning
#' stock biomass from EM in regards to the OM, and the year
#' @export
calc_standard_error_ssb <- function(em_output,
                                    om_output,
                                    col = "SE.SSB",
                                    quants = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                    inc_mean = TRUE){

  map_df(seq_along(em_output), ~{
    if(is.na(.x)){
      NA
    }else{
      yrs <- as.numeric(names(em_output[[.x]]$ssb_se[[length(em_output[[.x]]$ssb_se)]]))
      nyrs <- length(yrs)
      ssb_true <- rowSums(om_output[[.x]]$ssb)
      ssb_est <- em_output[[.x]]$ssb_se[length(em_output[[.x]]$ssb_se)][[1]]
      err <- (ssb_est - ssb_true) / ssb_true
      data.frame(run = rep(.x, nyrs),
                 ssb_se = err,
                 year = yrs) %>%
        as_tibble() %>%
        select(year, ssb_se, run)
    }
  })
  #%>%
    # calc_quantiles_by_group(grp_col = "year",
    #                         col = "ssb_se",
    #                         probs = quants,
    #                         include_mean = inc_mean)
}
