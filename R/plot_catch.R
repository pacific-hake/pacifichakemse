#' Plot Catch by scenario and country
#'
#' @param ps A plot setup object as output by [setup_mse_plot_objects()]
#' @param ci A vector of length two of the lower and upper credible interval values.
#' @param yr_lim A vector of 2 for minimum and maximum yrs to show on the plot. If either are NA,
#' the limits of the data are used

#' @return A [ggplot2::ggplot()] object
#' @export
plot_catch <- function(ps = NULL,
                       ci = c(0.05, 0.95),
                       yr_lim = c(NA_real_, NA_real_)){

  verify_argument(ps, "list")
  verify_argument(ci, "numeric", 2)
  verify_argument(yr_lim, "numeric", 2)

  catch <- ps$mse_quants$catch_quant
  stopifnot("0.5" %in% names(catch))
  stopifnot(all(ci %in% names(catch)))

  ci <- as.character(ci) %>% map(~{sym(.x)})

  g <- ggplot(catch,
              aes(x = year, y = `0.5` * 1e-6, color = scenario)) +
    geom_line(size = 1.5) +
    #geom_ribbon(aes(ymin = p5 * 1e-6, ymax = p95 * 1e-6), linetype = 2, fill = alpha(alpha =0.2, colour = ps$cols)) +
    scale_color_manual(values = ps$cols) +
    scale_y_continuous(name = "Catch (million tonnes)") +
    geom_line(aes(y = !!ci[[1]] * 1e-6, color = scenario), linetype = 2) +
    geom_line(aes(y = !!ci[[2]] * 1e-6, color = scenario), linetype = 2) +
    theme(legend.position = c(0.12, 0.8),
          legend.title = element_blank(),
          legend.box.background = element_blank()) +
    coord_cartesian(xlim = yr_lim)

  g
}