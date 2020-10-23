#' Plot Spawning Stock Biomass (SSB) by scenario and country
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()].
#' @param time Either 'beg' for beginning of the year SSB or 'mid' for mid-year SSB.
#' @param ci A vector of length two of the lower and upper credible interval values.
#' @param yr_lim A vector of 2 for minimum and maximum yrs to show on the plot. If either are NA,
#' the limits of the data are used
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_ssb <- function(ps = NULL,
                     time = "beg",
                     ci = c(0.05, 0.95),
                     yr_lim = c(NA_real_, NA_real_)){

  verify_argument(ps, "list")
  verify_argument(time, "character", 1, c("beg", "mid"))
  verify_argument(ci, "numeric", 2)
  verify_argument(yr_lim, "numeric", 2)

  if(time == "beg"){
    ssb <- ps$mse_quants$ssb_quant
  }else{
    ssb <- ps$mse_quants$ssb_mid_quant
  }
  stopifnot("country" %in% names(ssb))
  stopifnot("0.5" %in% names(ssb))
  stopifnot(all(ci %in% names(ssb)))

  ssb_can <- ssb %>% filter(country == "Canada")
  ssb_us <- ssb %>% filter(country == "US")

  ci <- as.character(ci) %>% map(~{sym(.x)})

  g <- ggplot(ssb_can, aes(x = year, y = `0.5` * 1e-6)) +
    geom_line(color = "red", size = 1.5) +
    geom_ribbon(aes(ymin = !!ci[[1]] * 1e-6, ymax = !!ci[[2]] * 1e-6),
                fill = alpha("red", alpha = 0.2), linetype = 0) +
    geom_line(aes(y = ssb_us$`0.5` * 1e-6), color = "blue", size = 1.5) +
    theme_classic() +
    scale_y_continuous(name ="SSB (million tonnes)") +
    facet_wrap(~scenario) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
    geom_ribbon(aes(ymin = ssb_us[[ci[[1]]]] * 1e-6, ymax = ssb_us[[ci[[2]]]] * 1e-6),
                fill = alpha("blue", alpha = 0.2), linetype = 0) +
    coord_cartesian(xlim = yr_lim)

  g
}