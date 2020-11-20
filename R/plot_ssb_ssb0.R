#' Plot total SSB/SSB0 by scenario
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param ci A vector of length two of the lower and upper credible interval values.
#' @param yr_lim A vector of 2 for minimum and maximum yrs to show on the plot. If either are NA,
#' the limits of the data are used
#'
#' @return A [ggplot2::ggplot()] object
#' @importFrom dplyr ungroup
#' @export
plot_ssb_ssb0 <- function(ps = NULL,
                          ci = c(0.05, 0.95),
                          yr_lim = c(NA_real_, NA_real_)){

  verify_argument(ps, "list")
  verify_argument(yr_lim, "numeric", 2)
  verify_argument(yr_lim, "numeric", 2)

  ssb_ssb0 <- ps$mse_quants$ssb_ssb0_quant

  stopifnot("0.5" %in% names(ssb_ssb0))
  stopifnot(is.numeric(ci))
  stopifnot(all(ci %in% names(ssb_ssb0)))
  stopifnot(length(ci) == 2)

  ci <- as.character(ci) %>% map(~{sym(.x)})

  cols <- ps$cols

  # Reorder the legend and colors
  ssb_ssb0 <- ssb_ssb0 %>%
    mutate(scenario = fct_relevel(scenario, rev(levels(ssb_ssb0$scenario))))

  subsc <- 0

  g <- ggplot(ssb_ssb0, aes(x = year, y = `0.5`, color = scenario, fill = scenario)) +
    geom_line(size = 1.5) +
    #geom_line(aes(y = !!ci[[1]]), linetype = 2, size = 1.2) +
    #geom_line(aes(y = !!ci[[2]]), linetype = 2, size = 1.2) +
    scale_color_manual(values = cols) +
    scale_y_continuous(name = bquote("Total SSB / SSB"[.(subsc)])) +
    coord_cartesian(ylim  = c(0, 4)) +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5),
          legend.position = c(0.1, 0.9),
          legend.title = element_blank()) +
    geom_ribbon(aes(ymin = !!ci[[1]], ymax = !!ci[[2]]), linetype = 0) +
    scale_fill_manual(values = alpha(cols, alpha = 0.2)) +
    geom_hline(aes(yintercept = 1), color = "black", linetype = 2) +
    coord_cartesian(xlim = yr_lim)

  g
}
